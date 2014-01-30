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

// nsSVGFilterInstance

nsSVGFilterInstance::nsSVGFilterInstance(
  nsIFrame* aTargetFrame,
  const gfxRect& aTargetBBox,
  const gfxMatrix& aUserSpaceToIntermediateSpaceTransform,
  const nsStyleFilter& aFilter,
  nsTArray<FilterPrimitiveDescription>& aPrimitiveDescrs,
  nsTArray<mozilla::RefPtr<SourceSurface>>& aInputImages) :
    mTargetFrame(aTargetFrame),
    mTargetBBox(aTargetBBox),
    mUserSpaceToIntermediateSpaceTransform(
      aUserSpaceToIntermediateSpaceTransform),
    mFilter(aFilter),
    mPrimitiveDescrs(aPrimitiveDescrs),
    mInputImages(aInputImages),
    mSourceAlphaAvailable(false),
    mImageTable(10),
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
  mPrimitiveUnits =
    mFilterFrame->GetEnumValue(SVGFilterElement::PRIMITIVEUNITS);

  // Compute the intermediate space to user space transform.
  mIntermediateSpaceToUserSpaceTransform =
    mUserSpaceToIntermediateSpaceTransform;
  mIntermediateSpaceToUserSpaceTransform.Invert();

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
  uint32_t initialNumPrimitives = mPrimitiveDescrs.Length();
  nsresult rv = BuildPrimitives();
  if (NS_FAILED(rv)) {
    return;
  }

  uint32_t finalNumPrimitives = mPrimitiveDescrs.Length();
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
    nsSVGUtils::GetRelativeRect(filterUnits, XYWH, mTargetBBox, mTargetFrame);

  // Match the filter region as closely as possible to the pixel density of the
  // nearest outer 'svg' device space:
  IntRect roundedIntermediateSpaceBounds =
    UserSpaceToIntermediateSpace(userSpaceBounds);
  gfxRect roundedUserSpaceBounds =
    IntermediateSpaceToUserSpace(roundedIntermediateSpaceBounds);
  return roundedUserSpaceBounds;
}

IntRect
nsSVGFilterInstance::UserSpaceToIntermediateSpace(
  const gfxRect& aUserSpace) const
{
  gfxRect intermediateSpace =
    mUserSpaceToIntermediateSpaceTransform.TransformBounds(aUserSpace);
  intermediateSpace.RoundOut();
  nsIntRect intermediateSpaceNsIntRect;
  bool overflow =
    !gfxUtils::GfxRectToIntRect(intermediateSpace, &intermediateSpaceNsIntRect);
  if (overflow) {
    return IntRect();
  }
  return ToIntRect(intermediateSpaceNsIntRect);
}

gfxRect
nsSVGFilterInstance::IntermediateSpaceToUserSpace(
  const IntRect& aIntermediateSpace) const
{
  gfxRect intermediateSpace = ToGfxRect(aIntermediateSpace);
  gfxRect userSpace =
    mIntermediateSpaceToUserSpaceTransform.TransformBounds(intermediateSpace);
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
  return IntermediateSpaceToUserSpace(aFilterSpace) +
    mUserSpaceBounds.TopLeft();
}


int32_t
nsSVGFilterInstance::GetCurrentResultIndex()
{
  uint32_t numPrimitiveDescrs = mPrimitiveDescrs.Length();
  return numPrimitiveDescrs <= 0 ?
    FilterPrimitiveDescription::kPrimitiveIndexSourceGraphic :
    numPrimitiveDescrs - 1;
}

nsresult
nsSVGFilterInstance::BuildPrimitives()
{
  ClipLastPrimitiveDescriptionByFilterRegion();
  
  // The SourceGraphic for this SVG filter is the result of previous SVG or CSS
  // filter, or the SourceGraphic if there is none.
  mSourceGraphicIndex = GetCurrentResultIndex();

  // Get the filter primitive elements.
  nsTArray<nsRefPtr<nsSVGFE> > primitiveElements;
  GetFilterPrimitiveElements(primitiveElements);

  for (uint32_t primitiveElementIndex = 0;
       primitiveElementIndex < primitiveElements.Length();
       primitiveElementIndex++) {
    nsSVGFE* primitiveElement = primitiveElements[primitiveElementIndex];

    nsAutoTArray<int32_t,2> sourceIndices;
    nsresult rv = GetOrCreateSourceIndicesForNextPrimitive(primitiveElement,
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

    AttachSources(descr, primitiveElement, sourceIndices);
    AppendPrimitiveDescription(descr, primitiveElement);
  }

  return NS_OK;
}

void
nsSVGFilterInstance::AppendPrimitiveDescription(
  const FilterPrimitiveDescription& aDescr,
  nsSVGFE* aPrimitiveElement)
{
  mPrimitiveDescrs.AppendElement(aDescr);

  nsAutoString resultName;
  aPrimitiveElement->GetResultImageName().GetAnimValue(resultName,
                                                       aPrimitiveElement);
  uint32_t primitiveDescrIndex = mPrimitiveDescrs.Length() - 1;
  mImageTable.Put(resultName, primitiveDescrIndex);
}

void
nsSVGFilterInstance::AttachSources(FilterPrimitiveDescription& aDescr,
                                  nsSVGFE* aPrimitiveElement,
                                   nsTArray<int32_t>& aSourceIndices)
{
  for (uint32_t i = 0; i < aSourceIndices.Length(); i++) {
    AttachSource(aDescr, aPrimitiveElement, i, aSourceIndices[i]);
  }

  // The output color space is whatever in1 is if there is an in1.
  ColorSpace outputColorSpace = aSourceIndices.Length() > 0 ?
    aDescr.InputColorSpace(0) :
    aPrimitiveElement->GetOutputColorSpace();
  aDescr.SetOutputColorSpace(outputColorSpace);
}

void
nsSVGFilterInstance::AttachSource(FilterPrimitiveDescription& aDescr,
                                  nsSVGFE* aPrimitiveElement,
                                  int32_t aInputIndex,
                                  int32_t aSourceIndex)
{
  aDescr.SetInputPrimitive(aInputIndex, aSourceIndex);

  ColorSpace inputColorSpace = aSourceIndex < 0 ? 
    SRGB :
    mPrimitiveDescrs[aSourceIndex].OutputColorSpace();
  ColorSpace desiredInputColorSpace =
    aPrimitiveElement->GetInputColorSpace(aInputIndex, inputColorSpace);
  aDescr.SetInputColorSpace(aInputIndex, desiredInputColorSpace);
}

void
nsSVGFilterInstance::GetFilterPrimitiveElements(
    nsTArray<nsRefPtr<nsSVGFE> >& aPrimitives) {
  for (nsIContent* child = mFilterElement->nsINode::GetFirstChild();
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
nsSVGFilterInstance::GetOrCreateSourceIndicesForNextPrimitive(
  nsSVGFE* aPrimitiveElement,
  nsTArray<int32_t>& aSourceIndices)
{
  nsAutoTArray<nsSVGStringInfo,2> sources;
  aPrimitiveElement->GetSourceImageNames(sources);

  for (uint32_t j = 0; j < sources.Length(); j++) {
    nsAutoString str;
    sources[j].mString->GetAnimValue(str, sources[j].mElement);

    int32_t sourceIndex = 0;
    if (str.EqualsLiteral("SourceGraphic")) {
      sourceIndex = mSourceGraphicIndex;
    } else if (str.EqualsLiteral("SourceAlpha")) {
      sourceIndex = GetOrCreateSourceAlphaIndex();
    } else if (str.EqualsLiteral("FillPaint")) {
      sourceIndex = FilterPrimitiveDescription::kPrimitiveIndexFillPaint;
    } else if (str.EqualsLiteral("StrokePaint")) {
      sourceIndex = FilterPrimitiveDescription::kPrimitiveIndexStrokePaint;
    } else if (str.EqualsLiteral("BackgroundImage") ||
               str.EqualsLiteral("BackgroundAlpha")) {
      return NS_ERROR_NOT_IMPLEMENTED;
    } else if (str.EqualsLiteral("")) {
      sourceIndex = GetCurrentResultIndex();
    } else {
      bool inputExists = mImageTable.Get(str, &sourceIndex);
      if (!inputExists)
        return NS_ERROR_FAILURE;
    }
    aSourceIndices.AppendElement(sourceIndex);
  }
  return NS_OK;
}

int32_t nsSVGFilterInstance::GetOrCreateSourceAlphaIndex()
{
  // Lazily return the SourceAlpha index, if we've already created a
  // FilterPrimitiveDescription that generates SourceAlpha or determined that
  // we can use the SourceAlpha of the unfiltered image.
  if (mSourceAlphaAvailable) {
    return mSourceAlphaIndex;
  }

  // If we are the first filter in the chain, we can just reference the
  // SourceAlpha of the unfiltered image.
  if (mSourceGraphicIndex < 0) {
    mSourceAlphaAvailable = true;
    mSourceAlphaIndex = FilterPrimitiveDescription::kPrimitiveIndexSourceAlpha;
    return mSourceAlphaIndex;
  }

  // Create a new PrimitiveDescription that will convert the SourceGraphic into
  // SourceAlpha.
  FilterPrimitiveDescription descr(
    FilterPrimitiveDescription::eComponentTransfer);

  // Feed the PrimitiveDescription that creates the SourceGraphic into the new
  // PrimitiveDescription that will create SourceAlpha.
  descr.SetInputPrimitive(0, mSourceGraphicIndex);

  const FilterPrimitiveDescription& sourcePrimitiveDescr =
    mPrimitiveDescrs[mSourceGraphicIndex];
  descr.SetPrimitiveSubregion(sourcePrimitiveDescr.PrimitiveSubregion());
  
  ColorSpace colorSpace = sourcePrimitiveDescr.OutputColorSpace();
  descr.SetInputColorSpace(0, colorSpace);
  descr.SetOutputColorSpace(colorSpace);

  // Zero out the RGB channels to black.
  static const AttributeName attributeNames[3] = {
    eComponentTransferFunctionR,
    eComponentTransferFunctionG,
    eComponentTransferFunctionB
  };

  for (int32_t i = 0; i < 3; i++) {
    AttributeMap functionAttributes;
    functionAttributes.Set(eComponentTransferFunctionType,
                           (uint32_t)SVG_FECOMPONENTTRANSFER_TYPE_TABLE);
    float tableValues[2] = {0.0, 0.0};
    functionAttributes.Set(eComponentTransferFunctionTableValues,
                           tableValues, 2);
    descr.Attributes().Set(attributeNames[i], functionAttributes);
  }

  // Keep the alpha channel untouched.
  AttributeMap functionAttributes;
  functionAttributes.Set(eComponentTransferFunctionType,
                         (uint32_t)SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY);
  descr.Attributes().Set(eComponentTransferFunctionA, functionAttributes);

  mPrimitiveDescrs.AppendElement(descr);
  mSourceAlphaAvailable = true;
  mSourceAlphaIndex = mPrimitiveDescrs.Length() - 1;
  return mSourceAlphaIndex;
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
        mPrimitiveDescrs[inputIndex].PrimitiveSubregion() :
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
  IntRect intermediateSpaceSubregion =
    UserSpaceToIntermediateSpace(userSpaceSubregion);

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
  uint32_t numPrimitiveDescrs = mPrimitiveDescrs.Length();
  if (numPrimitiveDescrs <= 0)
    return;

  FilterPrimitiveDescription& descr =
    mPrimitiveDescrs[numPrimitiveDescrs - 1];
  IntRect primitiveSubregion = descr.PrimitiveSubregion();
  primitiveSubregion =
    primitiveSubregion.Intersect(mIntermediateSpaceBounds);
  descr.SetPrimitiveSubregion(primitiveSubregion);
}

gfxRect
nsSVGFilterInstance::ToGfxRect(const IntRect& rect)
{
  return gfxRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}

IntRect
nsSVGFilterInstance::ToIntRect(const nsIntRect& rect)
{
  return IntRect(rect.X(), rect.Y(), rect.Width(), rect.Height());
}
