/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// Main header first:
#include "nsFilterInstance.h"

// Keep others in (case-insensitive) order:
#include "gfxPlatform.h"
#include "gfxUtils.h"
#include "nsISVGChildFrame.h"
#include "nsRenderingContext.h"
#include "mozilla/dom/SVGFilterElement.h"
#include "nsCSSFilterInstance.h"
#include "nsReferencedElement.h"
#include "nsSVGFilterInstance.h"
#include "nsSVGFilterPaintCallback.h"
#include "nsSVGUtils.h"
#include "SVGContentUtils.h"
#include "FilterSupport.h"
#include "gfx2DGlue.h"

using namespace mozilla;
using namespace mozilla::dom;
using namespace mozilla::gfx;

nsIntRect
nsFilterInstance::ToNsIntRect(const IntRect& rect)
{
  return nsIntRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

nsIntRect
nsFilterInstance::ToNsIntRect(const gfxRect& rect)
{
  return nsIntRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

IntRect
nsFilterInstance::ToIntRect(const gfxRect& rect)
{
  return IntRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

gfxRect
nsFilterInstance::ToGfxRect(const nsIntRect& rect)
{
  return gfxRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

gfxRect
nsFilterInstance::ToGfxRect(const IntRect& rect)
{
  return gfxRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

// TODO(mvujovic): Actually use INT_MAX, INT_MIN.
IntRect
nsFilterInstance::InfiniteIntRect()
{
  return IntRect(-5000, -5000, 10000, 10000);
}

/**
 * Converts an nsRect that is relative to a filtered frame's origin (i.e. the
 * top-left corner of its border box) into filter space.
 * Returns the entire filter region (a rect the width/height of aFilterRes) if
 * aFrameRect is null, or if the result is too large to be stored in an
 * nsIntRect.
 */
static nsIntRect
MapFrameRectToFilterSpace(const nsRect* aRect,
                          int32_t aAppUnitsPerCSSPx,
                          const gfxMatrix& aFrameSpaceInCSSPxToFilterSpace,
                          const gfxIntSize& aFilterRes)
{
  nsIntRect rect(0, 0, aFilterRes.width, aFilterRes.height);
  if (aRect) {
    if (aRect->IsEmpty()) {
      return nsIntRect();
    }
    gfxRect rectInCSSPx =
      nsLayoutUtils::RectToGfxRect(*aRect, aAppUnitsPerCSSPx);
    gfxRect rectInFilterSpace =
      aFrameSpaceInCSSPxToFilterSpace.TransformBounds(rectInCSSPx);
    rectInFilterSpace.RoundOut();
    nsIntRect intRect;
    if (gfxUtils::GfxRectToIntRect(rectInFilterSpace, &intRect)) {
      rect = intRect;
    }
  }
  return rect;
}

/**
 * Returns the transform from frame space to the coordinate space that
 * GetCanvasTM transforms to. "Frame space" is the origin of a frame, aka the
 * top-left corner of its border box, aka the top left corner of its mRect.
 */
static gfxMatrix
GetUserToFrameSpaceInCSSPxTransform(nsIFrame *aFrame)
{
  gfxMatrix userToFrameSpaceInCSSPx;

  if ((aFrame->GetStateBits() & NS_FRAME_SVG_LAYOUT)) {
    int32_t appUnitsPerCSSPx = aFrame->PresContext()->AppUnitsPerCSSPixel();
    // As currently implemented by Mozilla for the purposes of filters, user
    // space is the coordinate system established by GetCanvasTM(), since
    // that's what we use to set filterToDeviceSpace above. In other words,
    // for SVG, user space is actually the coordinate system aTarget
    // establishes for _its_ children (i.e. after taking account of any x/y
    // and viewBox attributes), not the coordinate system that is established
    // for it by its 'transform' attribute (or by its _parent_) as it's
    // normally defined. (XXX We should think about fixing this.) The only
    // frame type for which these extra transforms are not simply an x/y
    // translation is nsSVGInnerSVGFrame, hence we treat it specially here.
    if (aFrame->GetType() == nsGkAtoms::svgInnerSVGFrame) {
      userToFrameSpaceInCSSPx =
        static_cast<nsSVGElement*>(aFrame->GetContent())->
          PrependLocalTransformsTo(gfxMatrix());
    } else {
      gfxPoint targetsUserSpaceOffset =
        nsLayoutUtils::RectToGfxRect(aFrame->GetRect(), appUnitsPerCSSPx).
                         TopLeft();
      userToFrameSpaceInCSSPx.Translate(-targetsUserSpaceOffset);
    }
  }
  // else, for all other frames, leave as the identity matrix
  return userToFrameSpaceInCSSPx;
}

nsFilterInstance::nsFilterInstance(
  nsIFrame *aTarget,
  const nsTArray<nsStyleFilter>& aFilters,
  nsSVGFilterPaintCallback *aPaint,
  const nsRect *aPostFilterDirtyRect,
  const nsRect *aPreFilterDirtyRect,
  const nsRect *aPreFilterVisualOverflowRectOverride,
  const gfxRect *aOverrideBBox,
  nsIFrame* aTransformRoot)
{
  mInitialized = false;
  mTargetFrame = aTarget;
  mFilters = aFilters;
  mPaintCallback = aPaint;
  mTargetBBox = aOverrideBBox ? 
    *aOverrideBBox : nsSVGUtils::GetBBox(mTargetFrame);
  mTransformRoot = aTransformRoot;

  // Get the user space to filter space transform.
  mCanvasTransform =
    nsSVGUtils::GetCanvasTM(mTargetFrame, nsISVGChildFrame::FOR_OUTERSVG_TM);
  if (mCanvasTransform.IsSingular()) {
    // Nothing to draw.
    return;
  }

  // TODO(mvujovic): Get rid of this.
  nsresult rv = BuildPrimitives();
  if (NS_FAILED(rv)) {
    return;
  }

  if (mPrimitiveDescriptions.IsEmpty()) {
    // Nothing should be rendered, so nothing is needed.
    return;
  }

  ComputeOverallFilterMetrics();

  ConvertRectsFromFrameSpaceToFilterSpace(aPostFilterDirtyRect,
                                          aPreFilterDirtyRect,
                                          aPreFilterVisualOverflowRectOverride);

  mInitialized = true;
}

nsIntRect
nsFilterInstance::UserSpaceToIntermediateSpace(const gfxRect& aUserSpace,
                                               bool aRoundOut,
                                               bool* aOverflow) const
{
  NS_ASSERTION(!mCanvasTransform.IsSingular(),
    "we shouldn't be doing anything if canvas transform is singular");

  gfxRect filterSpace = aUserSpace;
  gfxSize scale = mCanvasTransform.ScaleFactors(true);
  filterSpace.Scale(scale.width, scale.height);
  if (aRoundOut) {
    filterSpace.RoundOut();
  }

  // Detect possible float->int overflow.
  nsIntRect filterSpaceInt;
  bool overflow = !gfxUtils::GfxRectToIntRect(filterSpace, &filterSpaceInt);
  if (aOverflow) {
    *aOverflow = overflow;
  }
  return overflow ? nsIntRect() : filterSpaceInt;
}

gfxRect
nsFilterInstance::IntermediateSpaceToUserSpace(
  const nsIntRect& aIntermediateSpace) const
{
  NS_ASSERTION(!mCanvasTransform.IsSingular(),
    "we shouldn't be doing anything if canvas transform is singular");

  gfxRect userSpace = ToGfxRect(aIntermediateSpace);
  gfxSize scale = mCanvasTransform.ScaleFactors(true);
  userSpace.Scale(1.0 / scale.width, 1.0 / scale.height);
  return userSpace;
}

nsIntRect
nsFilterInstance::UserSpaceToFilterSpace(const gfxRect& aUserSpace,
                                         bool aRoundOut,
                                         bool* aOverflow) const
{
  return UserSpaceToIntermediateSpace(aUserSpace - mUserSpaceBounds.TopLeft(),
                                      aRoundOut,
                                      aOverflow);
}

gfxRect
nsFilterInstance::FilterSpaceToUserSpace(const nsIntRect& aFilterSpace) const
{
  return IntermediateSpaceToUserSpace(aFilterSpace) +
         mUserSpaceBounds.TopLeft();
}

gfxPoint
nsFilterInstance::FilterSpaceToUserSpace(const gfxPoint& aPt) const
{
  return gfxPoint(aPt.x * mUserSpaceBounds.Width() / mFilterSpaceBounds.width + mUserSpaceBounds.X(),
                  aPt.y * mUserSpaceBounds.Height() / mFilterSpaceBounds.height + mUserSpaceBounds.Y());
}

nsresult
nsFilterInstance::BuildPrimitives()
{
  NS_ASSERTION(!mPrimitiveDescriptions.Length(),
    "expected to start building primitives from scratch");

  for (uint32_t i = 0; i < mFilters.Length(); i++) {
    if (NS_FAILED(BuildPrimitivesForFilter(mFilters[i]))) {
      mPrimitiveDescriptions.Clear();
      return NS_ERROR_FAILURE;
    }
  }
  return NS_OK;
}

nsresult
nsFilterInstance::BuildPrimitivesForFilter(const nsStyleFilter& filter)
{
  nsresult result = NS_ERROR_FAILURE;
  if (filter.GetType() == NS_STYLE_FILTER_URL) {
    nsSVGFilterInstance svgFilterInstance(mTargetFrame,
                                          mTargetBBox,
                                          filter,
                                          mPrimitiveDescriptions,
                                          mInputImages);
    result = svgFilterInstance.IsInitialized() ? NS_OK : NS_ERROR_FAILURE;
  } else {
    nsCSSFilterInstance cssFilterInstance(filter, mPrimitiveDescriptions);
    result = cssFilterInstance.IsInitialized() ? NS_OK : NS_ERROR_FAILURE;
  }
  return result;
}

void
nsFilterInstance::TranslatePrimitiveSubregions(IntPoint translation)
{
  for (uint32_t i = 0; i < mPrimitiveDescriptions.Length(); i++) {
    FilterPrimitiveDescription& primitiveDescription = 
      mPrimitiveDescriptions[i];
    IntRect primitiveSubregion = primitiveDescription.PrimitiveSubregion();
    primitiveSubregion += translation;
    primitiveDescription.SetPrimitiveSubregion(primitiveSubregion);
  }
}

void
nsFilterInstance::ComputeOverallFilterMetrics()
{
  // TODO(mvujovic): Follow ComputePostFilterExtents more closely. Check for overflow.

  // Compute intermediate space bounds.
  FilterDescription filterDescription(mPrimitiveDescriptions,
                                      InfiniteIntRect());
  nsIntRect sourceBounds = UserSpaceToIntermediateSpace(mTargetBBox);
  nsIntRegion postFilterExtents =
    FilterSupport::ComputePostFilterExtents(filterDescription, sourceBounds);
  mFilterSpaceBounds = postFilterExtents.GetBounds();

  // Compute filter region.
  mUserSpaceBounds = IntermediateSpaceToUserSpace(mFilterSpaceBounds);

  // Compute final filter space bounds.
  nsIntPoint filterSpaceOffset = mFilterSpaceBounds.TopLeft();
  mFilterSpaceBounds -= filterSpaceOffset;
  TranslatePrimitiveSubregions(-IntPoint(filterSpaceOffset.x, filterSpaceOffset.y));

  // Compute various transforms.

  // Compute filter space to user space transform.
  gfxMatrix filterToUserSpace(
    mUserSpaceBounds.Width() / mFilterSpaceBounds.Width(), 0.0f,
    0.0f, mUserSpaceBounds.Height() / mFilterSpaceBounds.height,
    mUserSpaceBounds.X(), mUserSpaceBounds.Y());

  // Compute filter space to device space transform. Only used when we paint.
  if (mPaintCallback) {
    mFilterSpaceToDeviceSpaceTransform = filterToUserSpace *
      nsSVGUtils::GetCanvasTM(mTargetFrame, nsISVGChildFrame::FOR_PAINTING);
  }

  // Compute filter space to frame space transform.
  mFilterSpaceToFrameSpaceInCSSPxTransform =
    filterToUserSpace * GetUserToFrameSpaceInCSSPxTransform(mTargetFrame);
}

void
nsFilterInstance::ConvertRectsFromFrameSpaceToFilterSpace(
  const nsRect *aPostFilterDirtyRect,
  const nsRect *aPreFilterDirtyRect,
  const nsRect *aPreFilterVisualOverflowRectOverride)
{
  // Compute frame space to filter space transform.
  // Note that filterToFrameSpaceInCSSPx is always invertible.
  gfxMatrix frameSpaceInCSSPxToFilterSpace =
    mFilterSpaceToFrameSpaceInCSSPxTransform;
  frameSpaceInCSSPxToFilterSpace.Invert();

  int32_t appUnitsPerCSSPx = mTargetFrame->PresContext()->AppUnitsPerCSSPixel();
  gfxIntSize filterRes = mFilterSpaceBounds.Size();

  mPostFilterDirtyRect =
    MapFrameRectToFilterSpace(aPostFilterDirtyRect, appUnitsPerCSSPx,
                              frameSpaceInCSSPxToFilterSpace, filterRes);
  mPreFilterDirtyRect =
    MapFrameRectToFilterSpace(aPreFilterDirtyRect, appUnitsPerCSSPx,
                              frameSpaceInCSSPxToFilterSpace, filterRes);
  nsIntRect preFilterVisualOverflowRect;
  if (aPreFilterVisualOverflowRectOverride) {
    preFilterVisualOverflowRect =
      MapFrameRectToFilterSpace(aPreFilterVisualOverflowRectOverride,
                                appUnitsPerCSSPx,
                                frameSpaceInCSSPxToFilterSpace, filterRes);
  } else {
    nsRect preFilterVOR = mTargetFrame->GetPreEffectsVisualOverflowRect();
    preFilterVisualOverflowRect =
      MapFrameRectToFilterSpace(&preFilterVOR, appUnitsPerCSSPx,
                                frameSpaceInCSSPxToFilterSpace, filterRes);
  }
  mTargetBounds = preFilterVisualOverflowRect;
}

void
nsFilterInstance::ComputeNeededBoxes()
{
  if (mPrimitiveDescriptions.IsEmpty())
    return;

  nsIntRegion sourceGraphicNeededRegion;
  nsIntRegion fillPaintNeededRegion;
  nsIntRegion strokePaintNeededRegion;

  FilterDescription filter(
    mPrimitiveDescriptions, ToIntRect(mFilterSpaceBounds));
  FilterSupport::ComputeSourceNeededRegions(
    filter, mPostFilterDirtyRect,
    sourceGraphicNeededRegion, fillPaintNeededRegion, strokePaintNeededRegion);

  bool overflow;
  nsIntRect sourceBounds = UserSpaceToFilterSpace(mTargetBBox, true, &overflow);
  if (overflow) {
    return;
  }
  sourceBounds.UnionRect(sourceBounds, mTargetBounds);

  sourceGraphicNeededRegion.And(sourceGraphicNeededRegion, sourceBounds);

  mSourceGraphic.mNeededBounds = sourceGraphicNeededRegion.GetBounds();
  mFillPaint.mNeededBounds = fillPaintNeededRegion.GetBounds();
  mStrokePaint.mNeededBounds = strokePaintNeededRegion.GetBounds();
}

nsresult
nsFilterInstance::BuildSourcePaint(SourceInfo *aSource,
                                      gfxASurface* aTargetSurface,
                                      DrawTarget* aTargetDT)
{
  nsIntRect neededRect = aSource->mNeededBounds;

  RefPtr<DrawTarget> offscreenDT;
  nsRefPtr<gfxASurface> offscreenSurface;
  nsRefPtr<gfxContext> ctx;
  if (aTargetSurface) {
    offscreenSurface = gfxPlatform::GetPlatform()->CreateOffscreenSurface(
      neededRect.Size(), GFX_CONTENT_COLOR_ALPHA);
    if (!offscreenSurface || offscreenSurface->CairoStatus()) {
      return NS_ERROR_OUT_OF_MEMORY;
    }
    ctx = new gfxContext(offscreenSurface);
  } else {
    offscreenDT = gfxPlatform::GetPlatform()->CreateOffscreenContentDrawTarget(
      ToIntSize(neededRect.Size()), FORMAT_B8G8R8A8);
    if (!offscreenDT) {
      return NS_ERROR_OUT_OF_MEMORY;
    }
    ctx = new gfxContext(offscreenDT);
  }

  ctx->Translate(-neededRect.TopLeft());

  nsRenderingContext tmpCtx;
  tmpCtx.Init(mTargetFrame->PresContext()->DeviceContext(), ctx);

  gfxRect r = FilterSpaceToUserSpace(mFilterSpaceBounds);

  gfxMatrix deviceToFilterSpace = GetFilterSpaceToDeviceSpaceTransform().Invert();
  gfxContext *gfx = tmpCtx.ThebesContext();
  gfx->Multiply(deviceToFilterSpace);

  gfx->Save();

  gfxMatrix matrix =
    nsSVGUtils::GetCanvasTM(mTargetFrame, nsISVGChildFrame::FOR_PAINTING,
                            mTransformRoot);
  if (!matrix.IsSingular()) {
    gfx->Multiply(matrix);
    gfx->Rectangle(r);
    if ((aSource == &mFillPaint && 
         nsSVGUtils::SetupCairoFillPaint(mTargetFrame, gfx)) ||
        (aSource == &mStrokePaint &&
         nsSVGUtils::SetupCairoStrokePaint(mTargetFrame, gfx))) {
      gfx->Fill();
    }
  }
  gfx->Restore();

  if (offscreenSurface) {
    aSource->mSourceSurface =
      gfxPlatform::GetPlatform()->GetSourceSurfaceForSurface(aTargetDT, offscreenSurface);
  } else {
    aSource->mSourceSurface = offscreenDT->Snapshot();
  }
  aSource->mSurfaceRect = ToIntRect(neededRect);

  return NS_OK;
}

nsresult
nsFilterInstance::BuildSourcePaints(gfxASurface* aTargetSurface,
                                       DrawTarget* aTargetDT)
{
  nsresult rv = NS_OK;

  if (!mFillPaint.mNeededBounds.IsEmpty()) {
    rv = BuildSourcePaint(&mFillPaint, aTargetSurface, aTargetDT);
    NS_ENSURE_SUCCESS(rv, rv);
  }

  if (!mStrokePaint.mNeededBounds.IsEmpty()) {
    rv = BuildSourcePaint(&mStrokePaint, aTargetSurface, aTargetDT);
    NS_ENSURE_SUCCESS(rv, rv);
  }
  return  rv;
}

nsresult
nsFilterInstance::BuildSourceImage(gfxASurface* aTargetSurface,
                                      DrawTarget* aTargetDT)
{
  nsIntRect neededRect = mSourceGraphic.mNeededBounds;
  if (neededRect.IsEmpty()) {
    return NS_OK;
  }

  RefPtr<DrawTarget> offscreenDT;
  nsRefPtr<gfxASurface> offscreenSurface;
  nsRefPtr<gfxContext> ctx;
  if (aTargetSurface) {
    offscreenSurface = gfxPlatform::GetPlatform()->CreateOffscreenSurface(
      neededRect.Size(), GFX_CONTENT_COLOR_ALPHA);
    if (!offscreenSurface || offscreenSurface->CairoStatus()) {
      return NS_ERROR_OUT_OF_MEMORY;
    }
    ctx = new gfxContext(offscreenSurface);
  } else {
    offscreenDT = gfxPlatform::GetPlatform()->CreateOffscreenContentDrawTarget(
      ToIntSize(neededRect.Size()), FORMAT_B8G8R8A8);
    if (!offscreenDT) {
      return NS_ERROR_OUT_OF_MEMORY;
    }
    ctx = new gfxContext(offscreenDT);
  }

  ctx->Translate(-neededRect.TopLeft());

  nsRenderingContext tmpCtx;
  tmpCtx.Init(mTargetFrame->PresContext()->DeviceContext(), ctx);

  gfxRect r = FilterSpaceToUserSpace(neededRect);
  r.RoundOut();
  nsIntRect dirty;
  if (!gfxUtils::GfxRectToIntRect(r, &dirty))
    return NS_ERROR_FAILURE;

  // SVG graphics paint to device space, so we need to set an initial device
  // space to filter space transform on the gfxContext that SourceGraphic
  // and SourceAlpha will paint to.
  //
  // (In theory it would be better to minimize error by having filtered SVG
  // graphics temporarily paint to user space when painting the sources and
  // only set a user space to filter space transform on the gfxContext
  // (since that would eliminate the transform multiplications from user
  // space to device space and back again). However, that would make the
  // code more complex while being hard to get right without introducing
  // subtle bugs, and in practice it probably makes no real difference.)
  gfxMatrix deviceToFilterSpace = GetFilterSpaceToDeviceSpaceTransform().Invert();
  tmpCtx.ThebesContext()->Multiply(deviceToFilterSpace);
  mPaintCallback->Paint(&tmpCtx, mTargetFrame, &dirty, mTransformRoot);

  RefPtr<SourceSurface> sourceGraphicSource;

  if (offscreenSurface) {
    sourceGraphicSource =
      gfxPlatform::GetPlatform()->GetSourceSurfaceForSurface(aTargetDT, offscreenSurface);
  } else {
    sourceGraphicSource = offscreenDT->Snapshot();
  }

  mSourceGraphic.mSourceSurface = sourceGraphicSource;
  mSourceGraphic.mSurfaceRect = ToIntRect(neededRect);
   
  return NS_OK;
}

nsresult
nsFilterInstance::Render(gfxContext* aContext)
{
  NS_ASSERTION(mInitialized, "filter instance must be initialized");

  nsIntRect filterRect = mPostFilterDirtyRect.Intersect(mFilterSpaceBounds);

  if (filterRect.IsEmpty()) {
    return NS_OK;
  }

  nsRefPtr<gfxASurface> resultImage;
  RefPtr<DrawTarget> resultImageDT;
  if (aContext->IsCairo()) {
    resultImage =
      gfxPlatform::GetPlatform()->CreateOffscreenSurface(filterRect.Size(),
                                                         GFX_CONTENT_COLOR_ALPHA);
    if (!resultImage || resultImage->CairoStatus())
      return NS_ERROR_OUT_OF_MEMORY;

    // Create a Cairo DrawTarget around resultImage.
    resultImageDT =
      gfxPlatform::GetPlatform()->CreateDrawTargetForSurface(
        resultImage, ToIntSize(filterRect.Size()));
  } else {
    resultImageDT = gfxPlatform::GetPlatform()->CreateOffscreenContentDrawTarget(
      ToIntSize(filterRect.Size()), FORMAT_B8G8R8A8);
  }

  ComputeNeededBoxes();

  nsresult rv = BuildSourceImage(resultImage, resultImageDT);
  if (NS_FAILED(rv))
    return rv;
  rv = BuildSourcePaints(resultImage, resultImageDT);
  if (NS_FAILED(rv))
    return rv;

  IntRect filterSpaceBounds = ToIntRect(mFilterSpaceBounds);
  FilterDescription filter(mPrimitiveDescriptions, filterSpaceBounds);

  FilterSupport::RenderFilterDescription(
    resultImageDT, filter, ToRect(filterRect),
    mSourceGraphic.mSourceSurface, mSourceGraphic.mSurfaceRect,
    mFillPaint.mSourceSurface, mFillPaint.mSurfaceRect,
    mStrokePaint.mSourceSurface, mStrokePaint.mSurfaceRect,
    mInputImages);

  RefPtr<SourceSurface> resultImageSource;
  if (!resultImage) {
    resultImageSource = resultImageDT->Snapshot();
  }

  gfxMatrix ctm = GetFilterSpaceToDeviceSpaceTransform();
  nsSVGUtils::CompositeSurfaceMatrix(aContext, resultImage, resultImageSource,
                                     filterRect.TopLeft(), ctm);

  return NS_OK;
}

nsresult
nsFilterInstance::ComputePostFilterDirtyRect(nsRect* aPostFilterDirtyRect)
{
  NS_ASSERTION(mInitialized, "filter instance must be initialized");

  if (mPreFilterDirtyRect.IsEmpty()) {
    *aPostFilterDirtyRect = nsRect();
    return NS_OK;
  }

  IntRect filterSpaceBounds = ToIntRect(mFilterSpaceBounds);
  FilterDescription filter(mPrimitiveDescriptions, filterSpaceBounds);
  nsIntRegion resultChangeRegion =
    FilterSupport::ComputeResultChangeRegion(filter,
      mPreFilterDirtyRect, nsIntRegion(), nsIntRegion());
  *aPostFilterDirtyRect = TransformFilterSpaceToFrameSpace(resultChangeRegion.GetBounds());

  return NS_OK;
}

nsresult
nsFilterInstance::ComputePostFilterExtents(nsRect* aPostFilterExtents)
{
  NS_ASSERTION(mInitialized, "filter instance must be initialized");

  bool overflow;
  nsIntRect sourceBounds = UserSpaceToFilterSpace(mTargetBBox, true, &overflow);
  if (overflow) {
    *aPostFilterExtents = nsRect();
    return NS_ERROR_FAILURE;
  }
  sourceBounds.UnionRect(sourceBounds, mTargetBounds);

  IntRect filterSpaceBounds = ToIntRect(mFilterSpaceBounds);
  FilterDescription filter(mPrimitiveDescriptions, filterSpaceBounds);
  nsIntRegion postFilterExtents =
    FilterSupport::ComputePostFilterExtents(filter, sourceBounds);
  *aPostFilterExtents = TransformFilterSpaceToFrameSpace(postFilterExtents.GetBounds());

  return NS_OK;
}

nsresult
nsFilterInstance::ComputeSourceNeededRect(nsRect* aDirty)
{
  NS_ASSERTION(mInitialized, "filter instance must be initialized");

  ComputeNeededBoxes();
  *aDirty = TransformFilterSpaceToFrameSpace(mSourceGraphic.mNeededBounds);

  return NS_OK;
}

nsRect
nsFilterInstance::TransformFilterSpaceToFrameSpace(const nsIntRect& aRect) const
{
  if (aRect.IsEmpty()) {
    return nsRect();
  }
  gfxRect r(aRect.x, aRect.y, aRect.width, aRect.height);
  r = GetFilterSpaceToFrameSpaceInCSSPxTransform().TransformBounds(r);
  return nsLayoutUtils::RoundGfxRectToAppRect(r, AppUnitsPerCSSPixel());
}
