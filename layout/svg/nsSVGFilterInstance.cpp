/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// Main header first:
#include "nsSVGFilterInstance.h"

// Keep others in (case-insensitive) order:
#include "nsISVGChildFrame.h"
#include "mozilla/dom/SVGFilterElement.h"
#include "nsReferencedElement.h"
#include "nsSVGFilterFrame.h"
#include "nsSVGFilters.h"
#include "nsSVGNumber2.h"
#include "nsSVGUtils.h"
#include "SVGContentUtils.h"
#include "FilterSupport.h"
#include "gfx2DGlue.h"

using namespace mozilla;
using namespace mozilla::dom;
using namespace mozilla::gfx;

IntRect
nsSVGFilterInstance::ToIntRect(const gfxRect& rect)
{
  return IntRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

gfxRect
nsSVGFilterInstance::ToGfxRect(const IntRect& rect)
{
  return gfxRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

// nsSVGFilterInstance

nsSVGFilterInstance::nsSVGFilterInstance(
  nsIFrame* aTargetFrame,
  const gfxRect& aTargetBBox,
  const nsStyleFilter& aFilter,
  nsTArray<FilterPrimitiveDescription>& aPrimitiveDescriptions,
  nsTArray<mozilla::RefPtr<SourceSurface>>& aInputImages) :
    mTargetFrame(aTargetFrame),
    mTargetBBox(aTargetBBox),
    mFilter(aFilter),
    mPrimitiveDescriptions(aPrimitiveDescriptions),
    mInputImages(aInputImages),
    mInitialized(false)
{
  // Get the filter frame.
  mFilterFrame = GetFilterFrame();
  if (!mFilterFrame) {
    return;
  }

  // Get the filter element.
  mFilterElement = mFilterFrame->GetFilterContent();
  if (!mFilterElement) {
    NS_NOTREACHED("filter frame should have a related element");
    return;
  }

  // Get the primitive units.
  mPrimitiveUnits = mFilterFrame->GetEnumValue(SVGFilterElement::PRIMITIVEUNITS);

  // Get the user space to filter space transform.
  mCanvasTransform =
    nsSVGUtils::GetCanvasTM(mTargetFrame, nsISVGChildFrame::FOR_OUTERSVG_TM);
  if (mCanvasTransform.IsSingular()) {
    // Nothing to draw.
    return;
  }

  // Compute the filter region (in various spaces).
  mUserSpaceBounds = ComputeUserSpaceBounds();
  if (mUserSpaceBounds.Width() <= 0 || mUserSpaceBounds.Height() <= 0) {
    // 0 disables rendering, < 0 is error. dispatch error console warning
    // or error as appropriate.
    return;
  }
  mIntermediateSpaceBounds = UserSpaceToIntermediateSpace(mUserSpaceBounds);
  mFilterSpaceBounds = UserSpaceToFilterSpace(mUserSpaceBounds);

  // Build the primitives.
  uint32_t initialNumPrimitives = mPrimitiveDescriptions.Length();
  nsresult rv = BuildPrimitives();
  if (NS_FAILED(rv)) {
    return;
  }

  uint32_t finalNumPrimitives = mPrimitiveDescriptions.Length();
  if (finalNumPrimitives == initialNumPrimitives) {
    // Nothing should be rendered, so nothing is needed.
    return;
  }

  mInitialized = true;
}

float 
nsSVGFilterInstance::GetPrimitiveNumber(
  uint8_t aCtxType,
  const nsSVGNumber2 *aNumber) const
{
  return GetPrimitiveNumber(aCtxType, aNumber->GetAnimValue());
}

float
nsSVGFilterInstance::GetPrimitiveNumber(
  uint8_t aCtxType, 
  const nsSVGNumberPair *aNumberPair,
  nsSVGNumberPair::PairIndex aIndex) const
{
  return GetPrimitiveNumber(aCtxType, aNumberPair->GetAnimValue(aIndex));
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
    return value * mFilterSpaceBounds.width / mUserSpaceBounds.Width();
  case SVGContentUtils::Y:
    return value * mFilterSpaceBounds.height / mUserSpaceBounds.Height();
  case SVGContentUtils::XY:
  default:
    return value * SVGContentUtils::ComputeNormalizedHypotenuse(
                     mFilterSpaceBounds.width / mUserSpaceBounds.Width(),
                     mFilterSpaceBounds.height / mUserSpaceBounds.Height());
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

  gfxRect userSpaceArea = nsSVGUtils::GetRelativeRect(
    mPrimitiveUnits, val, mTargetBBox, mTargetFrame);
  IntRect filterSpaceArea = UserSpaceToFilterSpace(userSpaceArea);
  return Point3D(filterSpaceArea.X(), filterSpaceArea.Y(),
    GetPrimitiveNumber(SVGContentUtils::XY, aPoint.z));
}

nsSVGFilterFrame*
nsSVGFilterInstance::GetFilterFrame()
{
  nsIURI* url = mFilter.GetURL();
  if (!url) {
    NS_NOTREACHED("expected a filter URL");
    return nullptr;
  }

  // Get the referenced filter element.
  nsReferencedElement filterElement;
  bool watch = false;
  filterElement.Reset(mTargetFrame->GetContent(), url, watch);
  Element* element = filterElement.get();
  if (!element) {
    // URL points to no element.
    return nullptr;
  }

  // Get the frame of the referenced filter element.
  nsIFrame* frame = element->GetPrimaryFrame();
  if (frame->GetType() != nsGkAtoms::svgFilterFrame) {
    // Not an SVG filter element.
    return nullptr;
  }
  return static_cast<nsSVGFilterFrame*>(frame);    
}

// TODO(mvujovic): Handle filterRes when there is a single SVG reference filter.
// Otherwise, ignore it.

// TODO(mvujovic): Reenable this for the overall filter space bounds.
// We don't care if this overflows, because we can handle upscaling /
// downscaling to filterRes.
// bool overflow;
// gfxSize filterRes = 
//   nsSVGUtils::ConvertToSurfaceSize(scaledSVGFilterRegion.Size(), &overflow);
// return IntRect(0, 0, filterRes.width, filterRes.height);

// Compute the user space bounds (aka filter region).
gfxRect
nsSVGFilterInstance::ComputeUserSpaceBounds()
{
  // XXX if filterUnits is set (or has defaulted) to objectBoundingBox, we
  // should send a warning to the error console if the author has used lengths
  // with units. This is a common mistake and can result in filterRes being
  // *massive* below (because we ignore the units and interpret the number as
  // a factor of the bbox width/height). We should also send a warning if the
  // user uses a number without units (a future SVG spec should really
  // deprecate that, since it's too confusing for a bare number to be sometimes
  // interpreted as a fraction of the bounding box and sometimes as user-space
  // units). So really only percentage values should be used in this case.

  // Get the filter region attributes from the filter element.
  nsSVGLength2 XYWH[4];
  NS_ABORT_IF_FALSE(sizeof(mFilterElement->mLengthAttributes) == sizeof(XYWH),
                    "XYWH size incorrect");
  memcpy(XYWH, 
         mFilterElement->mLengthAttributes,
         sizeof(mFilterElement->mLengthAttributes));
  XYWH[0] = *mFilterFrame->GetLengthValue(SVGFilterElement::ATTR_X);
  XYWH[1] = *mFilterFrame->GetLengthValue(SVGFilterElement::ATTR_Y);
  XYWH[2] = *mFilterFrame->GetLengthValue(SVGFilterElement::ATTR_WIDTH);
  XYWH[3] = *mFilterFrame->GetLengthValue(SVGFilterElement::ATTR_HEIGHT);

  // The filter region in user space, in user units:
  uint16_t filterUnits = 
    mFilterFrame->GetEnumValue(SVGFilterElement::FILTERUNITS);
  gfxRect userSpaceBounds = 
    nsSVGUtils::GetRelativeRect(filterUnits, XYWH, mTargetBBox, mFilterFrame);

  // Match the filter region as closely as possible to the pixel density of the
  // nearest outer 'svg' device space:
  return RoundOutUserSpace(userSpaceBounds);
}

gfxRect nsSVGFilterInstance::RoundOutUserSpace(const gfxRect& aUserSpace) const
{
  bool roundOut = true;
  IntRect roundedIntermediateSpaceBounds =
    UserSpaceToIntermediateSpace(aUserSpace, roundOut);
  gfxRect roundedUserSpaceBounds =
    IntermediateSpaceToUserSpace(roundedIntermediateSpaceBounds);
  return roundedUserSpaceBounds;
}

IntRect
nsSVGFilterInstance::UserSpaceToIntermediateSpace(
  const gfxRect& aUserSpace, bool aRoundOut) const
{
  NS_ASSERTION(!mCanvasTransform.IsSingular(),
    "we shouldn't be doing anything if canvas transform is singular");

  gfxRect filterSpace = aUserSpace;
  gfxSize scale = mCanvasTransform.ScaleFactors(true);
  filterSpace.Scale(scale.width, scale.height);
  if (aRoundOut)
    filterSpace.RoundOut();
  return ToIntRect(filterSpace);
}

gfxRect
nsSVGFilterInstance::IntermediateSpaceToUserSpace(
  const IntRect& aIntermediateSpace) const
{
  NS_ASSERTION(!mCanvasTransform.IsSingular(),
    "we shouldn't be doing anything if canvas transform is singular");

  gfxRect userSpace = ToGfxRect(aIntermediateSpace);
  gfxSize scale = mCanvasTransform.ScaleFactors(true);
  userSpace.Scale(1.0 / scale.width, 1.0 / scale.height);
  return userSpace;
}

IntRect
nsSVGFilterInstance::UserSpaceToFilterSpace(const gfxRect& aUserSpace) const
{
  return UserSpaceToIntermediateSpace(aUserSpace - mUserSpaceBounds.TopLeft());
}

gfxRect
nsSVGFilterInstance::FilterSpaceToUserSpace(const IntRect& aFilterSpace) const
{
  return IntermediateSpaceToUserSpace(aFilterSpace) + mUserSpaceBounds.TopLeft();
}

nsresult
nsSVGFilterInstance::BuildPrimitives()
{
  ClipLastPrimitiveDescriptionByFilterRegion();

  // Get the filter primitive elements.
  nsTArray<nsRefPtr<nsSVGFE> > primitiveElements;
  GetFilterPrimitiveElements(mFilterElement, primitiveElements);

  // Maps source image name to source index.
  nsDataHashtable<nsStringHashKey, int32_t> imageTable(10);

  for (uint32_t primitiveElementIndex = 0,
       primitiveDescriptionIndex = mPrimitiveDescriptions.Length();
       primitiveElementIndex < primitiveElements.Length();
       primitiveElementIndex++,
       primitiveDescriptionIndex++) {
    nsSVGFE* primitiveElement = primitiveElements[primitiveElementIndex];

    nsAutoTArray<int32_t,2> sourceIndices;
    nsresult rv = GetSourceIndices(primitiveElement,
                                   primitiveDescriptionIndex,
                                   imageTable,
                                   sourceIndices);
    if (NS_FAILED(rv)) {
      return rv;
    }

    IntRect primitiveSubregion =
      ComputeIntermediateSpacePrimitiveSubregion(primitiveElement,
                                                 sourceIndices);

    FilterPrimitiveDescription descr = 
      primitiveElement->GetPrimitiveDescription(this, 
                                                primitiveSubregion,
                                                mInputImages);

    descr.SetPrimitiveSubregion(primitiveSubregion);

    for (uint32_t i = 0; i < sourceIndices.Length(); i++) {
      int32_t inputIndex = sourceIndices[i];
      descr.SetInputPrimitive(i, inputIndex);
      ColorSpace inputColorSpace = inputIndex < 0 ? SRGB :
          mPrimitiveDescriptions[inputIndex].OutputColorSpace();
      ColorSpace desiredInputColorSpace =
        primitiveElement->GetInputColorSpace(i, inputColorSpace);
      descr.SetInputColorSpace(i, desiredInputColorSpace);
      if (i == 0) {
        // the output color space is whatever in1 is if there is an in1
        descr.SetOutputColorSpace(desiredInputColorSpace);
      }
    }

    if (sourceIndices.Length() == 0) {
      descr.SetOutputColorSpace(primitiveElement->GetOutputColorSpace());
    }

    mPrimitiveDescriptions.AppendElement(descr);

    nsAutoString str;
    primitiveElement->GetResultImageName().GetAnimValue(
      str, primitiveElement);
    imageTable.Put(str, primitiveDescriptionIndex);
  }

  return NS_OK;
}

void
nsSVGFilterInstance::GetFilterPrimitiveElements(
    const SVGFilterElement* aFilterElement, 
    nsTArray<nsRefPtr<nsSVGFE> >& aPrimitives) {
  for (nsIContent* child = aFilterElement->nsINode::GetFirstChild();
       child;
       child = child->GetNextSibling()) {
    nsRefPtr<nsSVGFE> primitive;
    CallQueryInterface(child, (nsSVGFE**)getter_AddRefs(primitive));
    if (primitive) {
      aPrimitives.AppendElement(primitive);
    }
  }  
}

nsresult
nsSVGFilterInstance::GetSourceIndices(
  nsSVGFE* aPrimitiveElement,
  int32_t aCurrentIndex,
  const nsDataHashtable<nsStringHashKey, int32_t>& aImageTable,
  nsTArray<int32_t>& aSourceIndices)
{
  nsAutoTArray<nsSVGStringInfo,2> sources;
  aPrimitiveElement->GetSourceImageNames(sources);

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

IntRect
nsSVGFilterInstance::ComputeIntermediateSpacePrimitiveSubregion(
  nsSVGFE* aPrimitiveElement,
  const nsTArray<int32_t>& aInputIndices)
{
  nsSVGFE* fE = aPrimitiveElement;

  IntRect defaultSubregion(0,0,0,0);
  if (fE->SubregionIsUnionOfRegions()) {
    for (uint32_t i = 0; i < aInputIndices.Length(); ++i) {
      int32_t inputIndex = aInputIndices[i];
      IntRect inputSubregion = inputIndex >= 0 ?
        mPrimitiveDescriptions[inputIndex].PrimitiveSubregion() :
        mIntermediateSpaceBounds;

      defaultSubregion = defaultSubregion.Union(inputSubregion);
    }
  } else {
    defaultSubregion = mIntermediateSpaceBounds;
  }

  uint16_t primitiveUnits =
    mFilterFrame->GetEnumValue(SVGFilterElement::PRIMITIVEUNITS);
  gfxRect userSpaceSubregion = 
    nsSVGUtils::GetRelativeRect(primitiveUnits,
                                &fE->mLengthAttributes[nsSVGFE::ATTR_X],
                                mTargetBBox,
                                mTargetFrame);

  // We currently require filter primitive subregions to be pixel-aligned.
  // Following the spec, any pixel partially in the subregion is included
  // in the subregion.
  bool roundOut = true;
  IntRect intermediateSpaceSubregion = 
    UserSpaceToIntermediateSpace(userSpaceSubregion, roundOut);

  if (!fE->mLengthAttributes[nsSVGFE::ATTR_X].IsExplicitlySet())
    intermediateSpaceSubregion.x = defaultSubregion.X();
  if (!fE->mLengthAttributes[nsSVGFE::ATTR_Y].IsExplicitlySet())
    intermediateSpaceSubregion.y = defaultSubregion.Y();
  if (!fE->mLengthAttributes[nsSVGFE::ATTR_WIDTH].IsExplicitlySet())
    intermediateSpaceSubregion.width = defaultSubregion.Width();
  if (!fE->mLengthAttributes[nsSVGFE::ATTR_HEIGHT].IsExplicitlySet())
    intermediateSpaceSubregion.height = defaultSubregion.Height();

  // Clip the primitive subregion to the filter region.
  intermediateSpaceSubregion.IntersectRect(intermediateSpaceSubregion,
                                           mIntermediateSpaceBounds);
  return intermediateSpaceSubregion;
}

void
nsSVGFilterInstance::ClipLastPrimitiveDescriptionByFilterRegion()
{
  uint32_t numPrimitiveDescriptions = mPrimitiveDescriptions.Length();
  if (numPrimitiveDescriptions <= 0)
    return;

  FilterPrimitiveDescription& descr =
    mPrimitiveDescriptions[numPrimitiveDescriptions - 1];
  IntRect primitiveSubregion = descr.PrimitiveSubregion();
  primitiveSubregion =
    primitiveSubregion.Intersect(mIntermediateSpaceBounds);
  descr.SetPrimitiveSubregion(primitiveSubregion);
}
