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

/**
 * This class helps nsFilterInstance build its filter graph.
 * This class processes a reference to an SVG <filter> element.
 * It iterates through the referenced <filter> element's primitive elements,
 * creating a FilterPrimitiveDescription for each one.
 * It appends the new FilterPrimitiveDescription(s) to the list of 
 * FilterPrimitiveDescriptions passed into the constructor.
 */
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
    const gfxMatrix& aUserSpaceToIntermediateSpaceTransform,
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

  /**
   * Gets the frame of the corresponding <filter> element.
   */
  nsSVGFilterFrame* GetFilterFrame();

  /**
   * Computes the SVG filter region in user space.
   *
   * The definition of "filter region" can be found here:
   * http://www.w3.org/TR/SVG11/filters.html#FilterEffectsRegion
   */
  gfxRect ComputeUserSpaceBounds();

  /**
   * Transforms a rectangle in user space to intermediate space.
   */
  IntRect UserSpaceToIntermediateSpace(
    const gfxRect& aUserSpace, bool aRoundOut = false) const;

  /**
   * Transforms a rectangle in intermediate space to user space.
   */
  gfxRect IntermediateSpaceToUserSpace(
    const IntRect& aIntermediateSpace) const;

  /**
   * Iterates through the <filter> element's primitive elements,
   * creating a FilterPrimitiveDescription for each one.
   * Appends the new FilterPrimitiveDescription(s) to the list of 
   * FilterPrimitiveDescriptions passed into the constructor.
   */
  nsresult BuildPrimitives();

  /**
   * Gets the children of the <filter> element (filter primitive elements).
   */
  void GetFilterPrimitiveElements(nsTArray<nsRefPtr<nsSVGFE> >& aPrimitives);

  /**
   * Gets the indices of the FilterPrimitiveDescription(s) or predefined sources
   * that feed into a filter primitive element.
   * 
   * Predefined sources include SourceGraphic, SourceAlpha, FillPaint, etc.
   * (e.g. <feGaussianBlur in="SourceGraphic" result="blur-result" ...>).
   *
   * If another filter primitive element is specified as the source
   * (e.g. <feColorMatrix in="blur-result" ...>), the index corresponding to its
   * FilterPrimitiveDescription is used.
   * 
   * If no source is specified, the previous FilterPrimitiveDescription is used
   * as the source. If there is no previous FilterPrimitiveDescription,
   * SourceGraphic is used.
   */
  nsresult GetSourceIndices(
    nsSVGFE* aPrimitiveElement,
    uint32_t& aCurrentIndex,
    const nsDataHashtable<nsStringHashKey, int32_t>& aImageTable,
    nsTArray<int32_t>& aSourceIndices);

  /**
   * Computes the filter primitive subregion for a filter primitive element,
   * in intermediate space.
   *
   * The definition of "filter primitive subregion" can be found here:
   * http://dev.w3.org/fxtf/filters/#FilterPrimitiveSubRegion
   */
  IntRect ComputeIntermediateSpacePrimitiveSubregion(
    nsSVGFE* aPrimitiveElement,
    const nsTArray<int32_t>& aInputIndices);

  /**
   * Finds the last FilterPrimitiveDescription in the list and clips its
   * filter primitive subregion to the filter region.
   */
  void ClipLastPrimitiveDescriptionByFilterRegion();

  /**
   * Appends a new FilterPrimitiveDescription to the
   * FilterPrimitiveDescription(s) list that converts the last result into
   * a SourceAlpha input for the next FilterPrimitiveDescription.
   *
   * This zeros out the RGB channels and keeps the alpha channel.
   */
  void AppendAlphaConversionPrimitiveDescription();

  static IntRect ToIntRect(const gfxRect& rect);
  static gfxRect ToGfxRect(const IntRect& rect);

  nsIFrame* mTargetFrame;
  gfxRect mTargetBBox;

  /**
   * The transforms between user space and intermediate space.
   */
  gfxMatrix mUserSpaceToIntermediateSpaceTransform;
  gfxMatrix mIntermediateSpaceToUserSpaceTransform;

  nsStyleFilter mFilter;
  nsTArray<FilterPrimitiveDescription>& mPrimitiveDescriptions;
  nsTArray<mozilla::RefPtr<SourceSurface>>& mInputImages;
  bool mInitialized;

  nsSVGFilterFrame* mFilterFrame;
  const SVGFilterElement* mFilterElement;

  /**
   * The bounds of the <filter> element's result, in different spaces.
   */
  gfxRect mUserSpaceBounds;
  IntRect mIntermediateSpaceBounds;
  IntRect mFilterSpaceBounds;

  /**
   * The 'primitiveUnits' attribute value (objectBoundingBox or userSpaceOnUse).
   */
  uint16_t mPrimitiveUnits;
};

#endif
