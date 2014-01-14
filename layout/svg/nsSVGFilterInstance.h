/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef __NS_SVGFILTERINSTANCE_H__
#define __NS_SVGFILTERINSTANCE_H__

#include "gfxMatrix.h"
#include "gfxRect.h"
#include "nsSVGNumberPair.h"
#include "nsTArray.h"
#include "nsIFrame.h"

class nsIFrame;
class nsSVGFE;
class nsSVGFilterFrame;
class nsSVGNumber2;

namespace mozilla {
namespace gfx {
class FilterPrimitiveDescription;
}
namespace dom {
class SVGFilterElement;
}
}

class nsSVGFilterInstance
{
  typedef mozilla::dom::SVGFilterElement SVGFilterElement;
  typedef mozilla::gfx::FilterPrimitiveDescription FilterPrimitiveDescription;
  typedef mozilla::gfx::IntRect IntRect;
  typedef mozilla::gfx::Point3D Point3D;
  typedef mozilla::gfx::SourceSurface SourceSurface;

public:
  nsSVGFilterInstance(
    nsIFrame* aTargetFrame,
    const gfxRect& aTargetBBox,
    const nsStyleFilter& aFilter,
    nsTArray<FilterPrimitiveDescription>& aPrimitiveDescriptions,
    nsTArray<mozilla::RefPtr<SourceSurface>>& aInputImages);

  bool IsInitialized() const { return mInitialized; }

  /**
   * Returns the user specified "filter region", in the filtered element's user
   * space, after it has been adjusted out (if necessary) so that its edges
   * coincide with pixel boundaries of the offscreen surface into which the
   * filtered output would/will be painted.
   */
  gfxRect GetFilterRegion() const { return mUserSpaceBounds; }

  float GetPrimitiveNumber(uint8_t aCtxType, const nsSVGNumber2 *aNumber) const;
  float GetPrimitiveNumber(uint8_t aCtxType,
                           const nsSVGNumberPair *aNumberPair,
                           nsSVGNumberPair::PairIndex aIndex) const;

  /**
   * Converts a userSpaceOnUse/objectBoundingBoxUnits unitless point
   * into filter space, depending on the value of mPrimitiveUnits. (For
   * objectBoundingBoxUnits, the bounding box offset is applied to the point.)
   */
  Point3D ConvertLocation(const Point3D& aPoint) const;

  IntRect UserSpaceToFilterSpace(const gfxRect& aUserSpace) const;
  gfxRect FilterSpaceToUserSpace(const IntRect& aFilterSpace) const;

private:
  /**
   * Scales a numeric filter primitive length in the X, Y or "XY" directions
   * into a length in filter space (no offset is applied).
   */
  float GetPrimitiveNumber(uint8_t aCtxType, float aValue) const;

  nsSVGFilterFrame* GetFilterFrame(nsIURI* url);
  gfxRect ComputeUserSpaceBounds();
  IntRect UserSpaceToIntermediateSpace(
    const gfxRect& aUserSpace, bool aRoundOut = false) const;
  gfxRect IntermediateSpaceToUserSpace(
    const IntRect& aIntermediateSpace) const;
  gfxRect RoundOutUserSpace(const gfxRect& aUserSpace) const;
  nsresult BuildPrimitives();
  void GetFilterPrimitiveElements(
    const SVGFilterElement* aFilterElement, 
    nsTArray<nsRefPtr<nsSVGFE> >& aPrimitives);
  static nsresult GetSourceIndices(
    nsSVGFE* aPrimitiveElement,
    int32_t aCurrentIndex,
    const nsDataHashtable<nsStringHashKey, int32_t>& aImageTable,
    nsTArray<int32_t>& aSourceIndices);
  IntRect ComputeIntermediateSpacePrimitiveSubregion(
    nsSVGFE* aPrimitiveElement,
    const nsTArray<int32_t>& aInputIndices);
  void ClipLastPrimitiveDescriptionByFilterRegion();

  static IntRect ToIntRect(const gfxRect& rect);
  static gfxRect ToGfxRect(const IntRect& rect);

  nsIFrame* mTargetFrame;
  gfxRect mTargetBBox;
  nsStyleFilter mFilter;
  nsTArray<FilterPrimitiveDescription>& mPrimitiveDescriptions;
  nsTArray<mozilla::RefPtr<SourceSurface>>& mInputImages;
  bool mInitialized;

  nsSVGFilterFrame* mFilterFrame;
  const SVGFilterElement* mFilterElement;
  gfxMatrix mCanvasTransform;
  gfxRect mUserSpaceBounds;
  IntRect mIntermediateSpaceBounds;
  IntRect mFilterSpaceBounds;

  /**
   * The 'primitiveUnits' attribute value (objectBoundingBox or userSpaceOnUse).
   */
  uint16_t mPrimitiveUnits;
};

#endif
