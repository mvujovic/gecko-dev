/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// Main header first:
#include "nsSVGFilterInstance.h"

// Keep others in (case-insensitive) order:
#include "gfxPlatform.h"
#include "gfxUtils.h"
#include "nsISVGChildFrame.h"
#include "nsRenderingContext.h"
#include "mozilla/dom/SVGFilterElement.h"
#include "nsSVGFilterPaintCallback.h"
#include "nsSVGUtils.h"
#include "SVGContentUtils.h"
#include "FilterSupport.h"
#include "gfx2DGlue.h"

using namespace mozilla;
using namespace mozilla::dom;
using namespace mozilla::gfx;

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

nsSVGFilterInstance::nsSVGFilterInstance(
  nsIFrame *aTarget,
  const nsTArray<nsStyleFilter>& aFilters,
  nsSVGFilterPaintCallback *aPaint,
  const nsRect *aPostFilterDirtyRect,
  const nsRect *aPreFilterDirtyRect,
  const nsRect *aPreFilterVisualOverflowRectOverride,
  const gfxRect *aOverrideBBox,
  nsIFrame* aTransformRoot)
{
  nsSVGFilterFrame* filterFrame = nsSVGEffects::GetFirstFilterFrame(aTarget);
  if (!filterFrame)
    return;

  Initialize(aTarget,
             filterFrame,
             aPaint,
             aPostFilterDirtyRect,
             aPreFilterDirtyRect,
             aPreFilterVisualOverflowRectOverride,
             aOverrideBBox,
             aTransformRoot);
}

void
nsSVGFilterInstance::Initialize(
  nsIFrame *aTarget,
  nsSVGFilterFrame *aFilterFrame,
  nsSVGFilterPaintCallback *aPaint,
  const nsRect *aPostFilterDirtyRect,
  const nsRect *aPreFilterDirtyRect,
  const nsRect *aPreFilterVisualOverflowRectOverride,
  const gfxRect *aOverrideBBox,
  nsIFrame* aTransformRoot)
{
  mInitialized = false;

  const SVGFilterElement *filter = aFilterFrame->GetFilterContent();

  uint16_t filterUnits =
    aFilterFrame->GetEnumValue(SVGFilterElement::FILTERUNITS);
  uint16_t primitiveUnits =
    aFilterFrame->GetEnumValue(SVGFilterElement::PRIMITIVEUNITS);

  gfxRect bbox = aOverrideBBox ? *aOverrideBBox : nsSVGUtils::GetBBox(aTarget);

  // Get the filter region (in the filtered element's user space):

  // XXX if filterUnits is set (or has defaulted) to objectBoundingBox, we
  // should send a warning to the error console if the author has used lengths
  // with units. This is a common mistake and can result in filterRes being
  // *massive* below (because we ignore the units and interpret the number as
  // a factor of the bbox width/height). We should also send a warning if the
  // user uses a number without units (a future SVG spec should really
  // deprecate that, since it's too confusing for a bare number to be sometimes
  // interpreted as a fraction of the bounding box and sometimes as user-space
  // units). So really only percentage values should be used in this case.
  
  nsSVGLength2 XYWH[4];
  NS_ABORT_IF_FALSE(sizeof(filter->mLengthAttributes) == sizeof(XYWH),
                    "XYWH size incorrect");
  memcpy(XYWH, filter->mLengthAttributes, sizeof(filter->mLengthAttributes));
  XYWH[0] = *aFilterFrame->GetLengthValue(SVGFilterElement::ATTR_X);
  XYWH[1] = *aFilterFrame->GetLengthValue(SVGFilterElement::ATTR_Y);
  XYWH[2] = *aFilterFrame->GetLengthValue(SVGFilterElement::ATTR_WIDTH);
  XYWH[3] = *aFilterFrame->GetLengthValue(SVGFilterElement::ATTR_HEIGHT);
  // The filter region in user space, in user units:
  gfxRect filterRegion = nsSVGUtils::GetRelativeRect(filterUnits,
    XYWH, bbox, aTarget);

  if (filterRegion.Width() <= 0 || filterRegion.Height() <= 0) {
    // 0 disables rendering, < 0 is error. dispatch error console warning
    // or error as appropriate.
    return;
  }

  // Calculate filterRes (the width and height of the pixel buffer of the
  // temporary offscreen surface that we would/will create to paint into when
  // painting the entire filtered element) and, if necessary, adjust
  // filterRegion out slightly so that it aligns with pixel boundaries of this
  // buffer:

  gfxIntSize filterRes;
  const nsSVGIntegerPair* filterResAttrs =
    aFilterFrame->GetIntegerPairValue(SVGFilterElement::FILTERRES);
  if (filterResAttrs->IsExplicitlySet()) {
    int32_t filterResX = filterResAttrs->GetAnimValue(nsSVGIntegerPair::eFirst);
    int32_t filterResY = filterResAttrs->GetAnimValue(nsSVGIntegerPair::eSecond);
    if (filterResX <= 0 || filterResY <= 0) {
      // 0 disables rendering, < 0 is error. dispatch error console warning?
      return;
    }

    filterRegion.Scale(filterResX, filterResY);
    filterRegion.RoundOut();
    filterRegion.Scale(1.0 / filterResX, 1.0 / filterResY);
    // We don't care if this overflows, because we can handle upscaling/
    // downscaling to filterRes
    bool overflow;
    filterRes =
      nsSVGUtils::ConvertToSurfaceSize(gfxSize(filterResX, filterResY),
                                       &overflow);
    // XXX we could send a warning to the error console if the author specified
    // filterRes doesn't align well with our outer 'svg' device space.
  } else {
    // Match filterRes as closely as possible to the pixel density of the nearest
    // outer 'svg' device space:
    gfxMatrix canvasTM =
      nsSVGUtils::GetCanvasTM(aTarget, nsISVGChildFrame::FOR_OUTERSVG_TM);
    if (canvasTM.IsSingular()) {
      // nothing to draw
      return;
    }

    gfxSize scale = canvasTM.ScaleFactors(true);
    filterRegion.Scale(scale.width, scale.height);
    filterRegion.RoundOut();
    // We don't care if this overflows, because we can handle upscaling/
    // downscaling to filterRes
    bool overflow;
    filterRes = nsSVGUtils::ConvertToSurfaceSize(filterRegion.Size(),
                                                 &overflow);
    filterRegion.Scale(1.0 / scale.width, 1.0 / scale.height);
  }

  // Get various transforms:

  gfxMatrix filterToUserSpace(filterRegion.Width() / filterRes.width, 0.0f,
                              0.0f, filterRegion.Height() / filterRes.height,
                              filterRegion.X(), filterRegion.Y());

  // Only used (so only set) when we paint:
  gfxMatrix filterToDeviceSpace;
  if (aPaint) {
    filterToDeviceSpace = filterToUserSpace *
              nsSVGUtils::GetCanvasTM(aTarget, nsISVGChildFrame::FOR_PAINTING);
  }

  // Convert the passed in rects from frame to filter space:

  int32_t appUnitsPerCSSPx = aTarget->PresContext()->AppUnitsPerCSSPixel();

  gfxMatrix filterToFrameSpaceInCSSPx =
    filterToUserSpace * GetUserToFrameSpaceInCSSPxTransform(aTarget);
  // filterToFrameSpaceInCSSPx is always invertible
  gfxMatrix frameSpaceInCSSPxTofilterSpace = filterToFrameSpaceInCSSPx;
  frameSpaceInCSSPxTofilterSpace.Invert();

  nsIntRect postFilterDirtyRect =
    MapFrameRectToFilterSpace(aPostFilterDirtyRect, appUnitsPerCSSPx,
                              frameSpaceInCSSPxTofilterSpace, filterRes);
  nsIntRect preFilterDirtyRect =
    MapFrameRectToFilterSpace(aPreFilterDirtyRect, appUnitsPerCSSPx,
                              frameSpaceInCSSPxTofilterSpace, filterRes);
  nsIntRect preFilterVisualOverflowRect;
  if (aPreFilterVisualOverflowRectOverride) {
    preFilterVisualOverflowRect =
      MapFrameRectToFilterSpace(aPreFilterVisualOverflowRectOverride,
                                appUnitsPerCSSPx,
                                frameSpaceInCSSPxTofilterSpace, filterRes);
  } else {
    nsRect preFilterVOR = aTarget->GetPreEffectsVisualOverflowRect();
    preFilterVisualOverflowRect =
      MapFrameRectToFilterSpace(&preFilterVOR, appUnitsPerCSSPx,
                                frameSpaceInCSSPxTofilterSpace, filterRes);
  }

  // Setup instance data
  mTargetFrame = aTarget;
  mPaintCallback = aPaint;
  mFilterElement = filter;
  mTargetBBox = bbox;
  mFilterSpaceToDeviceSpaceTransform = filterToDeviceSpace;
  mFilterSpaceToFrameSpaceInCSSPxTransform = filterToFrameSpaceInCSSPx;
  mFilterRegion = filterRegion;
  mFilterSpaceBounds = nsIntRect(0, 0, filterRes.width, filterRes.height);
  mTargetBounds = preFilterVisualOverflowRect;
  mPostFilterDirtyRect = postFilterDirtyRect;
  mPreFilterDirtyRect = preFilterDirtyRect;
  mPrimitiveUnits = primitiveUnits;
  mTransformRoot = aTransformRoot;

  // Build the primitives.
  nsresult rv = BuildPrimitives();
  if (NS_FAILED(rv)) {
    return;
  }

  if (mPrimitiveDescriptions.IsEmpty()) {
    // Nothing should be rendered, so nothing is needed.
    return;
  }

  mInitialized = true;
}

float
nsSVGFilterInstance::GetPrimitiveNumber(uint8_t aCtxType, float aValue) const
{
  nsSVGLength2 val;
  val.Init(aCtxType, 0xff, aValue,
           nsIDOMSVGLength::SVG_LENGTHTYPE_NUMBER);

  float value;
  if (mPrimitiveUnits == SVG_UNIT_TYPE_OBJECTBOUNDINGBOX) {
    value = nsSVGUtils::ObjectSpace(mTargetBBox, &val);
  } else {
    value = nsSVGUtils::UserSpace(mTargetFrame, &val);
  }

  switch (aCtxType) {
  case SVGContentUtils::X:
    return value * mFilterSpaceBounds.width / mFilterRegion.Width();
  case SVGContentUtils::Y:
    return value * mFilterSpaceBounds.height / mFilterRegion.Height();
  case SVGContentUtils::XY:
  default:
    return value * SVGContentUtils::ComputeNormalizedHypotenuse(
                     mFilterSpaceBounds.width / mFilterRegion.Width(),
                     mFilterSpaceBounds.height / mFilterRegion.Height());
  }
}

Point3D
nsSVGFilterInstance::ConvertLocation(const Point3D& aPoint) const
{
  nsSVGLength2 val[4];
  val[0].Init(SVGContentUtils::X, 0xff, aPoint.x,
              nsIDOMSVGLength::SVG_LENGTHTYPE_NUMBER);
  val[1].Init(SVGContentUtils::Y, 0xff, aPoint.y,
              nsIDOMSVGLength::SVG_LENGTHTYPE_NUMBER);
  // Dummy width/height values
  val[2].Init(SVGContentUtils::X, 0xff, 0,
              nsIDOMSVGLength::SVG_LENGTHTYPE_NUMBER);
  val[3].Init(SVGContentUtils::Y, 0xff, 0,
              nsIDOMSVGLength::SVG_LENGTHTYPE_NUMBER);

  gfxRect feArea = nsSVGUtils::GetRelativeRect(mPrimitiveUnits,
    val, mTargetBBox, mTargetFrame);
  gfxRect r = UserSpaceToFilterSpace(feArea);
  return Point3D(r.x, r.y, GetPrimitiveNumber(SVGContentUtils::XY, aPoint.z));
}

gfxRect
nsSVGFilterInstance::UserSpaceToFilterSpace(const gfxRect& aRect) const
{
  gfxRect r = aRect - mFilterRegion.TopLeft();
  r.Scale(mFilterSpaceBounds.width / mFilterRegion.Width(),
          mFilterSpaceBounds.height / mFilterRegion.Height());
  return r;
}

gfxPoint
nsSVGFilterInstance::FilterSpaceToUserSpace(const gfxPoint& aPt) const
{
  return gfxPoint(aPt.x * mFilterRegion.Width() / mFilterSpaceBounds.width + mFilterRegion.X(),
                  aPt.y * mFilterRegion.Height() / mFilterSpaceBounds.height + mFilterRegion.Y());
}

gfxMatrix
nsSVGFilterInstance::GetUserSpaceToFilterSpaceTransform() const
{
  gfxFloat widthScale = mFilterSpaceBounds.width / mFilterRegion.Width();
  gfxFloat heightScale = mFilterSpaceBounds.height / mFilterRegion.Height();
  return gfxMatrix(widthScale, 0.0f,
                   0.0f, heightScale,
                   -mFilterRegion.X() * widthScale, -mFilterRegion.Y() * heightScale);
}

IntRect
nsSVGFilterInstance::ComputeFilterPrimitiveSubregion(nsSVGFE* aFilterElement,
                                                     const nsTArray<int32_t>& aInputIndices)
{
  nsSVGFE* fE = aFilterElement;

  IntRect defaultFilterSubregion(0,0,0,0);
  if (fE->SubregionIsUnionOfRegions()) {
    for (uint32_t i = 0; i < aInputIndices.Length(); ++i) {
      int32_t inputIndex = aInputIndices[i];
      IntRect inputSubregion = inputIndex >= 0 ?
        mPrimitiveDescriptions[inputIndex].PrimitiveSubregion() :
        ToIntRect(mFilterSpaceBounds);

      defaultFilterSubregion = defaultFilterSubregion.Union(inputSubregion);
    }
  } else {
    defaultFilterSubregion = ToIntRect(mFilterSpaceBounds);
  }

  gfxRect feArea = nsSVGUtils::GetRelativeRect(mPrimitiveUnits,
    &fE->mLengthAttributes[nsSVGFE::ATTR_X], mTargetBBox, mTargetFrame);
  Rect region = ToRect(UserSpaceToFilterSpace(feArea));

  if (!fE->mLengthAttributes[nsSVGFE::ATTR_X].IsExplicitlySet())
    region.x = defaultFilterSubregion.X();
  if (!fE->mLengthAttributes[nsSVGFE::ATTR_Y].IsExplicitlySet())
    region.y = defaultFilterSubregion.Y();
  if (!fE->mLengthAttributes[nsSVGFE::ATTR_WIDTH].IsExplicitlySet())
    region.width = defaultFilterSubregion.Width();
  if (!fE->mLengthAttributes[nsSVGFE::ATTR_HEIGHT].IsExplicitlySet())
    region.height = defaultFilterSubregion.Height();

  // We currently require filter primitive subregions to be pixel-aligned.
  // Following the spec, any pixel partially in the region is included
  // in the region.
  region.RoundOut();

  return RoundedToInt(region);
}

static nsresult
GetSourceIndices(nsSVGFE* aFilterElement,
                 int32_t aCurrentIndex,
                 const nsDataHashtable<nsStringHashKey, int32_t>& aImageTable,
                 nsTArray<int32_t>& aSourceIndices)
{
  nsAutoTArray<nsSVGStringInfo,2> sources;
  aFilterElement->GetSourceImageNames(sources);

  for (uint32_t j = 0; j < sources.Length(); j++) {
    nsAutoString str;
    sources[j].mString->GetAnimValue(str, sources[j].mElement);

    int32_t sourceIndex = 0;
    if (str.EqualsLiteral("SourceGraphic")) {
      sourceIndex = FilterPrimitiveDescription::kPrimitiveIndexSourceGraphic;
    } else if (str.EqualsLiteral("SourceAlpha")) {
      sourceIndex = FilterPrimitiveDescription::kPrimitiveIndexSourceAlpha;
    } else if (str.EqualsLiteral("FillPaint")) {
      sourceIndex = FilterPrimitiveDescription::kPrimitiveIndexFillPaint;
    } else if (str.EqualsLiteral("StrokePaint")) {
      sourceIndex = FilterPrimitiveDescription::kPrimitiveIndexStrokePaint;
    } else if (str.EqualsLiteral("BackgroundImage") ||
               str.EqualsLiteral("BackgroundAlpha")) {
      return NS_ERROR_NOT_IMPLEMENTED;
    } else if (str.EqualsLiteral("")) {
      sourceIndex = aCurrentIndex == 0 ?
        FilterPrimitiveDescription::kPrimitiveIndexSourceGraphic :
        aCurrentIndex - 1;
    } else {
      bool inputExists = aImageTable.Get(str, &sourceIndex);
      if (!inputExists)
        return NS_ERROR_FAILURE;
    }

    MOZ_ASSERT(sourceIndex < aCurrentIndex);
    aSourceIndices.AppendElement(sourceIndex);
  }
  return NS_OK;
}

nsresult
nsSVGFilterInstance::BuildPrimitives()
{
  nsTArray<nsRefPtr<nsSVGFE> > primitives;
  for (nsIContent* child = mFilterElement->nsINode::GetFirstChild();
       child;
       child = child->GetNextSibling()) {
    nsRefPtr<nsSVGFE> primitive;
    CallQueryInterface(child, (nsSVGFE**)getter_AddRefs(primitive));
    if (primitive) {
      primitives.AppendElement(primitive);
    }
  }

  // Maps source image name to source index.
  nsDataHashtable<nsStringHashKey, int32_t> imageTable(10);

  for (uint32_t i = 0; i < primitives.Length(); ++i) {
    nsSVGFE* filter = primitives[i];

    nsAutoTArray<int32_t,2> sourceIndices;
    nsresult rv = GetSourceIndices(filter, i, imageTable, sourceIndices);
    if (NS_FAILED(rv)) {
      return rv;
    }

    IntRect primitiveSubregion =
      ComputeFilterPrimitiveSubregion(filter, sourceIndices);

    FilterPrimitiveDescription descr =
      filter->GetPrimitiveDescription(this, primitiveSubregion, mInputImages);

    descr.SetPrimitiveSubregion(primitiveSubregion);

    for (uint32_t j = 0; j < sourceIndices.Length(); j++) {
      int32_t inputIndex = sourceIndices[j];
      descr.SetInputPrimitive(j, inputIndex);
      ColorSpace inputColorSpace =
        inputIndex < 0 ? SRGB : mPrimitiveDescriptions[inputIndex].OutputColorSpace();
      ColorSpace desiredInputColorSpace = filter->GetInputColorSpace(j, inputColorSpace);
      descr.SetInputColorSpace(j, desiredInputColorSpace);
      if (j == 0) {
        // the output color space is whatever in1 is if there is an in1
        descr.SetOutputColorSpace(desiredInputColorSpace);
      }
    }

    if (sourceIndices.Length() == 0) {
      descr.SetOutputColorSpace(filter->GetOutputColorSpace());
    }

    mPrimitiveDescriptions.AppendElement(descr);

    nsAutoString str;
    filter->GetResultImageName().GetAnimValue(str, filter);
    imageTable.Put(str, i);
  }

  return NS_OK;
}

void
nsSVGFilterInstance::ComputeNeededBoxes()
{
  if (mPrimitiveDescriptions.IsEmpty())
    return;

  nsIntRegion sourceGraphicNeededRegion;
  nsIntRegion fillPaintNeededRegion;
  nsIntRegion strokePaintNeededRegion;

  FilterDescription filter(mPrimitiveDescriptions, ToIntRect(mFilterSpaceBounds));
  FilterSupport::ComputeSourceNeededRegions(
    filter, mPostFilterDirtyRect,
    sourceGraphicNeededRegion, fillPaintNeededRegion, strokePaintNeededRegion);

  nsIntRect sourceBoundsInt;
  gfxRect sourceBounds = UserSpaceToFilterSpace(mTargetBBox);
  sourceBounds.RoundOut();
  // Detect possible float->int overflow
  if (!gfxUtils::GfxRectToIntRect(sourceBounds, &sourceBoundsInt))
    return;
  sourceBoundsInt.UnionRect(sourceBoundsInt, mTargetBounds);

  sourceGraphicNeededRegion.And(sourceGraphicNeededRegion, sourceBoundsInt);

  mSourceGraphic.mNeededBounds = sourceGraphicNeededRegion.GetBounds();
  mFillPaint.mNeededBounds = fillPaintNeededRegion.GetBounds();
  mStrokePaint.mNeededBounds = strokePaintNeededRegion.GetBounds();
}

nsresult
nsSVGFilterInstance::BuildSourcePaint(SourceInfo *aSource,
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

  gfxMatrix m = GetUserSpaceToFilterSpaceTransform();
  m.Invert();
  gfxRect r = m.TransformBounds(mFilterSpaceBounds);

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
nsSVGFilterInstance::BuildSourcePaints(gfxASurface* aTargetSurface,
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
nsSVGFilterInstance::BuildSourceImage(gfxASurface* aTargetSurface,
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

  gfxMatrix m = GetUserSpaceToFilterSpaceTransform();
  m.Invert();
  gfxRect r = m.TransformBounds(neededRect);
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
nsSVGFilterInstance::Render(gfxContext* aContext)
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
nsSVGFilterInstance::ComputePostFilterDirtyRect(nsIntRect* aPostFilterDirtyRect)
{
  NS_ASSERTION(mInitialized, "filter instance must be initialized");

  *aPostFilterDirtyRect = nsIntRect();
  if (mPreFilterDirtyRect.IsEmpty()) {
    return NS_OK;
  }

  IntRect filterSpaceBounds = ToIntRect(mFilterSpaceBounds);
  FilterDescription filter(mPrimitiveDescriptions, filterSpaceBounds);
  nsIntRegion resultChangeRegion =
    FilterSupport::ComputeResultChangeRegion(filter,
      mPreFilterDirtyRect, nsIntRegion(), nsIntRegion());
  *aPostFilterDirtyRect = resultChangeRegion.GetBounds();
  return NS_OK;
}

nsresult
nsSVGFilterInstance::ComputePostFilterExtents(nsIntRect* aPostFilterExtents)
{
  NS_ASSERTION(mInitialized, "filter instance must be initialized");

  *aPostFilterExtents = nsIntRect();

  nsIntRect sourceBoundsInt;
  gfxRect sourceBounds = UserSpaceToFilterSpace(mTargetBBox);
  sourceBounds.RoundOut();
  // Detect possible float->int overflow
  if (!gfxUtils::GfxRectToIntRect(sourceBounds, &sourceBoundsInt))
    return NS_ERROR_FAILURE;
  sourceBoundsInt.UnionRect(sourceBoundsInt, mTargetBounds);

  IntRect filterSpaceBounds = ToIntRect(mFilterSpaceBounds);
  FilterDescription filter(mPrimitiveDescriptions, filterSpaceBounds);
  nsIntRegion postFilterExtents =
    FilterSupport::ComputePostFilterExtents(filter, sourceBoundsInt);
  *aPostFilterExtents = postFilterExtents.GetBounds();
  return NS_OK;
}

nsresult
nsSVGFilterInstance::ComputeSourceNeededRect(nsIntRect* aDirty)
{
  NS_ASSERTION(mInitialized, "filter instance must be initialized");

  ComputeNeededBoxes();
  *aDirty = mSourceGraphic.mNeededBounds;

  return NS_OK;
}
