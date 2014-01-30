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

nsresult
nsSVGFilterInstance::BuildPrimitives()
{
  ClipLastPrimitiveDescriptionByFilterRegion();

  // Get the filter primitive elements.
  nsTArray<nsRefPtr<nsSVGFE> > primitiveElements;
  GetFilterPrimitiveElements(primitiveElements);

  // Maps source image name to source index.
  nsDataHashtable<nsStringHashKey, int32_t> imageTable(10);

  uint32_t numPrimitiveDescrs = mPrimitiveDescrs.Length();
  int32_t sourceGraphicIndex = numPrimitiveDescrs > 0 ?
    (int32_t)numPrimitiveDescrs - 1 :
    FilterPrimitiveDescription::kPrimitiveIndexSourceGraphic;

  bool sourceAlphaAvailable = false;
  int32_t sourceAlphaIndex = 0;

  for (uint32_t primitiveElementIndex = 0,
       primitiveDescrIndex = numPrimitiveDescrs;
       primitiveElementIndex < primitiveElements.Length();
       primitiveElementIndex++,
       primitiveDescrIndex++) {
    nsSVGFE* primitiveElement = primitiveElements[primitiveElementIndex];

    nsAutoTArray<int32_t,2> sourceIndices;
    nsresult rv = GetSourceIndices(primitiveElement,
                                   primitiveDescrIndex,
                                   sourceGraphicIndex,
                                   sourceAlphaAvailable,
                                   sourceAlphaIndex,
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
          mPrimitiveDescrs[inputIndex].OutputColorSpace();
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

    mPrimitiveDescrs.AppendElement(descr);

    nsAutoString str;
    primitiveElement->GetResultImageName().GetAnimValue(
      str, primitiveElement);
    imageTable.Put(str, primitiveDescrIndex);
  }

  return NS_OK;
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
nsSVGFilterInstance::GetSourceIndices(
  nsSVGFE* aPrimitiveElement,
  uint32_t& aCurrentIndex,
  int32_t aSourceGraphicIndex,
  bool &aSourceAlphaAvailable,
  int32_t &aSourceAlphaIndex,
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
        sourceIndex = aSourceGraphicIndex;
    } else if (str.EqualsLiteral("SourceAlpha")) {
      if (aSourceAlphaAvailable) {
        sourceIndex = aSourceAlphaIndex;
      } else {
        if (aCurrentIndex == 0) {
          aSourceAlphaIndex =
            FilterPrimitiveDescription::kPrimitiveIndexSourceAlpha;
        } else {
          AppendAlphaConversionPrimitiveDescription(aSourceGraphicIndex);
          aSourceAlphaIndex = aCurrentIndex;
          aCurrentIndex++;
        }
        sourceIndex = aSourceAlphaIndex;
        aSourceAlphaAvailable = true;
      }
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

    MOZ_ASSERT(sourceIndex < (int32_t)aCurrentIndex);
    aSourceIndices.AppendElement(sourceIndex);
  }
  return NS_OK;
}

void nsSVGFilterInstance::AppendAlphaConversionPrimitiveDescription(
  int32_t aSourceGraphicIndex)
{
  FilterPrimitiveDescription descr(
    FilterPrimitiveDescription::eComponentTransfer);

  // Connect the new primitive description to the last one.
  IntRect primitiveSubregion = mIntermediateSpaceBounds;
  ColorSpace colorSpace = SRGB;
  if (aSourceGraphicIndex >= 0) {
    const FilterPrimitiveDescription& sourcePrimitiveDescr =
      mPrimitiveDescrs[aSourceGraphicIndex];
    primitiveSubregion = sourcePrimitiveDescr.PrimitiveSubregion();
    colorSpace = sourcePrimitiveDescr.OutputColorSpace();
  }

  descr.SetPrimitiveSubregion(primitiveSubregion);
  descr.SetInputPrimitive(0, aSourceGraphicIndex);
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
