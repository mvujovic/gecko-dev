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
    nsTArray<FilterPrimitiveDescription>& aPrimitiveDescrs,
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
  IntRect UserSpaceToIntermediateSpace(const gfxRect& aUserSpace) const;

  /**
   * Transforms a rectangle in intermediate space to user space.
   */
  gfxRect IntermediateSpaceToUserSpace(const IntRect& aIntermediateSpace) const;

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
   * When there are multiple chained SVG filters
   * (e.g. filter: url(#filter1) url(#filter2);), SourceGraphic and SourceAlpha
   * refer to the output of the previous CSS or SVG filter.
   *
   * If another filter primitive element is specified as the source
   * (e.g. <feColorMatrix in="blur-result" ...>), the index corresponding to its
   * FilterPrimitiveDescription is used.
   * 
   * If no source is specified, the previous FilterPrimitiveDescription is used
   * as the source. If there is no previous FilterPrimitiveDescription,
   * SourceGraphic is used.
   */
  nsresult GetOrCreateSourceIndicesForNextPrimitive(
    nsSVGFE* aPrimitiveElement,
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
   * FilterPrimitiveDescription(s) list that converts the SourceGraphic into
   * a SourceAlpha input for the next FilterPrimitiveDescription.
   *
   * This zeros out the RGB channels and keeps the alpha channel.
   */
  int32_t GetOrCreateSourceAlphaIndex();

  /**
   * Get the index of the last FilterPrimitiveDescription in the list,
   * or the SourceGraphic, if there are no FilterPrimitiveDescriptions yet.
   */
  int32_t GetCurrentResultIndex();

  /**
   * Maps a FilterPrimitiveDescription's input indices to source indices.
   *
   * The source index references another FilterPrimitiveDescription or a keyword
   * like SourceGraphic.
   *
   * This also sets the appropriate colors spaces for the inputs and the output.
   */
  void AttachSourcesToPrimitiveDescription(FilterPrimitiveDescription& aDescr,
                                           nsSVGFE* aPrimitiveElement,
                                           nsTArray<int32_t>& aSourceIndices);

  /**
   * Maps a FilterPrimitiveDescription's input index to a source index.
   *
   * The source index references another FilterPrimitiveDescription or a keyword
   * like SourceGraphic.
   *
   * This also sets the appropriate color space for the the input.
   */
  void AttachSourceToPrimitiveDescription(FilterPrimitiveDescription& aDescr,
                                          nsSVGFE* aPrimitiveElement,
                                          int32_t aInputIndex,
                                          int32_t aSourceIndex);

  /**
   * Adds a freshly minted FilterPrimitiveDescription to the
   * FilterPrimitiveDescription(s) list and adds its result name to a map.
   */
  void AppendPrimitiveDescription(const FilterPrimitiveDescription& aDescr,
                                  nsSVGFE* aPrimitiveElement);

  /**
   * Rect helpers.
   */
  static gfxRect ToGfxRect(const IntRect& rect);
  static IntRect ToIntRect(const nsIntRect& rect);

  /**
   * The frame for the element that is currently being filtered.
   */
  nsIFrame* mTargetFrame;

  /**
   * The SVG bbox of the element that is being filtered, in user space.
   */
  gfxRect mTargetBBox;

  /**
   * The transforms between user space and intermediate space.
   */
  gfxMatrix mUserSpaceToIntermediateSpaceTransform;
  gfxMatrix mIntermediateSpaceToUserSpaceTransform;

  /**
   * The bounds of the filter region, in different spaces.
   */
  gfxRect mUserSpaceBounds;
  IntRect mIntermediateSpaceBounds;
  IntRect mFilterSpaceBounds;

  /**
   * The "primitiveUnits" attribute value (objectBoundingBox or userSpaceOnUse).
   */
  uint16_t mPrimitiveUnits;

  /**
   * The style information for the SVG filter. Includes the SVG filter URL.
   */
  nsStyleFilter mFilter;

  /**
   * The list of FilterPrimitiveDescription(s), populated with primitives 
   * from previous SVG and CSS filters in the filter chain.
   *
   * This SVG filter will add its primitives to the list.
   */
  nsTArray<FilterPrimitiveDescription>& mPrimitiveDescrs;

  /**
   * The list of feImages we've collected from previous SVG and CSS filters in
   * the filter chain.
   *
   * This SVG filter will add its own feImages to the list.
   */
  nsTArray<mozilla::RefPtr<SourceSurface>>& mInputImages;

  /**
   * The index of the FilterPrimitiveDescription that this SVG filter should use
   * as its SourceGraphic, or the SourceGraphic keyword index
   * (kPrimitiveIndexSourceGraphic) if there are no FilterPrimitiveDescriptions.
   */
  int32_t mSourceGraphicIndex;

  /**
   * True if we've determined the SourceAlpha index.
   */
  bool mSourceAlphaAvailable;

  /**
   * The index of the FilterPrimitiveDescription we've created that generates
   * the SourceAlpha image.
   *
   * If we're the first filter in the chain, this will be the keyword index for
   * SourceAlpha (kPrimitiveIndexSourceAlpha).
   */
  int32_t mSourceAlphaIndex;

  /**
   * Maps result names to FilterPrimitiveDescription indexes.
   * (e.g. <filter result="resultName" ...>)
   */
  nsDataHashtable<nsStringHashKey, int32_t> mImageTable;

  /**
   * True if this filter instance was successfully initialized.
   */
  bool mInitialized;

  /**
   * The frame and element for the <filter> element.
   */
  nsSVGFilterFrame* mFilterFrame;
  const SVGFilterElement* mFilterElement;

};

#endif
