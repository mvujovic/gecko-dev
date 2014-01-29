/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef __NS_FILTERINSTANCE_H__
#define __NS_FILTERINSTANCE_H__

#include "gfxMatrix.h"
#include "gfxPoint.h"
#include "gfxRect.h"
#include "nsCOMPtr.h"
#include "nsHashKeys.h"
#include "nsPoint.h"
#include "nsRect.h"
#include "nsSize.h"
#include "nsSVGFilterFrame.h"
#include "nsSVGFilters.h"
#include "nsSVGNumber2.h"
#include "nsSVGNumberPair.h"
#include "nsTArray.h"
#include "nsIFrame.h"
#include "mozilla/gfx/2D.h"

class gfxASurface;
class nsIFrame;
class nsSVGFilterPaintCallback;

/**
 * This class performs all filter processing.
 * 
 * We build a graph of the filter image data flow, essentially
 * converting the filter graph to SSA. This lets us easily propagate
 * analysis data (such as bounding-boxes) over the filter primitive graph.
 *
 * Definition of "filter space": filter space is a coordinate system that is
 * aligned with the user space of the filtered element, with its origin located
 * at the top left of the filter region, and with one unit equal in size to one
 * pixel of the offscreen surface into which the filter output would/will be
 * painted.
 *
 * The definition of "filter region" can be found here:
 * http://www.w3.org/TR/SVG11/filters.html#FilterEffectsRegion
 */
class nsFilterInstance
{
  typedef mozilla::gfx::Point3D Point3D;
  typedef mozilla::gfx::IntPoint IntPoint;
  typedef mozilla::gfx::IntRect IntRect;
  typedef mozilla::gfx::SourceSurface SourceSurface;
  typedef mozilla::gfx::DrawTarget DrawTarget;
  typedef mozilla::gfx::FilterPrimitiveDescription FilterPrimitiveDescription;

public:
  nsFilterInstance(nsIFrame *aTarget,
                   const nsTArray<nsStyleFilter>& aFilters,
                   nsSVGFilterPaintCallback *aPaint,
                   const nsRect *aPostFilterDirtyRect,
                   const nsRect *aPreFilterDirtyRect,
                   const nsRect *aPreFilterVisualOverflowRectOverride,
                   const gfxRect *aOverrideBBox = nullptr,
                   nsIFrame* aTransformRoot = nullptr);

  bool IsInitialized() const { return mInitialized; }

  /**
   * Returns the user specified "filter region", in the filtered element's user
   * space, after it has been adjusted out (if necessary) so that its edges
   * coincide with pixel boundaries of the offscreen surface into which the
   * filtered output would/will be painted.
   */
  gfxRect GetFilterRegion() const { return mUserSpaceBounds; }

  /**
   * Draws the filter output into aContext. The area that
   * needs to be painted must have been specified before calling this method
   * by passing it as the aPostFilterDirtyRect argument to the
   * nsFilterInstance constructor.
   */
  nsresult Render(gfxContext* aContext);

  /**
   * Sets the aPostFilterDirtyRect outparam to the post-filter bounds in frame
   * space of the area that would be dirtied by mTargetFrame when a given
   * pre-filter area of mTargetFrame is dirtied. The pre-filter area must have
   * been specified before calling this method by passing it as the
   * aPreFilterDirtyRect argument to the nsFilterInstance constructor.
   */
  nsresult ComputePostFilterDirtyRect(nsRect* aPostFilterDirtyRect);

  /**
   * Sets the aPostFilterExtents outparam to the post-filter bounds in frame
   * space for the whole filter output. This is not necessarily equivalent to
   * the area that would be dirtied in the result when the entire pre-filter
   * area is dirtied, because some filter primitives can generate output
   * without any input.
   */
  nsresult ComputePostFilterExtents(nsRect* aPostFilterExtents);

  /**
   * Sets the aDirty outparam to the pre-filter bounds in frame space of the
   * area of mTargetFrame that is needed in order to paint the filtered output
   * for a given post-filter dirtied area. The post-filter area must have been
   * specified before calling this method by passing it as the
   * aPostFilterDirtyRect argument to the nsFilterInstance constructor.
   */
  nsresult ComputeSourceNeededRect(nsRect* aDirty);

private:
  struct SourceInfo {
    // Specifies which parts of the source need to be rendered.
    // Set by ComputeNeededBoxes().
    nsIntRect mNeededBounds;

    // The surface that contains the input rendering.
    // Set by BuildSourceImage / BuildSourcePaint.
    mozilla::RefPtr<SourceSurface> mSourceSurface;

    // The position and size of mSourceSurface in filter space.
    // Set by BuildSourceImage / BuildSourcePaint.
    IntRect mSurfaceRect;
  };

  /**
   * Creates a SourceSurface for either the FillPaint or StrokePaint graph
   * nodes
   */
  nsresult BuildSourcePaint(SourceInfo *aPrimitive,
                            gfxASurface* aTargetSurface,
                            DrawTarget* aTargetDT);

  /**
   * Creates a SourceSurface for either the FillPaint and StrokePaint graph
   * nodes, fills its contents and assigns it to mFillPaint.mSourceSurface and
   * mStrokePaint.mSourceSurface respectively.
   */
  nsresult BuildSourcePaints(gfxASurface* aTargetSurface,
                             DrawTarget* aTargetDT);

  /**
   * Creates the SourceSurface for the SourceGraphic graph node, paints its
   * contents, and assigns it to mSourceGraphic.mSourceSurface.
   */
  nsresult BuildSourceImage(gfxASurface* aTargetSurface,
                            DrawTarget* aTargetDT);

  /**
   * Build the list of FilterPrimitiveDescriptions with the nsStyleFilter chain.
   *
   * This populates mPrimitiveDescriptions and mInputImages.
   */
  nsresult BuildPrimitives();

  /**
   * Build the list of FilterPrimitiveDescriptions for a particular SVG
   * reference filter or CSS filter.
   *
   * This populates mPrimitiveDescriptions and mInputImages.
   */
  nsresult BuildPrimitivesForFilter(const nsStyleFilter& aFilter);

  /**
   * Computes the filter space bounds of the areas that we actually *need* from
   * the filter sources, based on the value of mPostFilterDirtyRect.
   * This sets mNeededBounds on the corresponding SourceInfo structs.
   */
  void ComputeNeededBoxes();

  /**
   * Translates all of the primitive descriptions' subregions
   * by a certain amount.
   */
  void TranslatePrimitiveSubregions(IntPoint aTranslation);

  /**
   * Computes the filter region based on the built primitive descriptions.
   * Also, computes various transforms for converting between spaces.
   */
  nsresult ComputeOverallFilterMetrics();


  /**
   * Sets this filter instance's pre and post filter dirty rects.
   */
  void ConvertRectsFromFrameSpaceToFilterSpace(
    const nsRect *aPostFilterDirtyRect,
    const nsRect *aPreFilterDirtyRect,
    const nsRect *aPreFilterVisualOverflowRectOverride);

  /**
   * The following set of functions convert rectangles from one space
   * (i.e. coordinate system) to another.
   *
   * The spaces are defined as follows:
   *
   * "user space"
   *    The filtered element's SVG user space or HTML CSS pixel space.
   *
   * "intermediate space"
   *    User space scaled to device pixels.
   *    Shares the same origin as user space.
   *    Filters with outsets (such as gaussian blur) may extend into the
   *    negative regions of this space.
   *    This space is used for convenience while building the filter primitive
   *    graph, so that all filter primitives are created with respect to the
   *    same origin.
   *
   * "filter space"
   *    Intermediate space translated so to the origin of the filtered result.
   *    Filters with outsets (such as gaussian blur) will be fully contained
   *    within the positive regions of this space.
   *    This space is computed after the filter primitive graph is built.
   * 
   * "frame space"
   *    The filtered element's frame space in app units.
   *
   * "frame space in CSS pixels"
   *    The filtered element's frame space in CSS pixels.
   *
   */

  /**
   * Transforms a rectangle in user space to filter space.
   */
  nsIntRect UserSpaceToFilterSpace(const gfxRect& aUserSpace,
                                   bool* aOverflow = nullptr) const;

  /**
   * Transforms a rectangle in filter space to user space.
   */
  gfxRect FilterSpaceToUserSpace(const nsIntRect& aFilterSpace) const;

  /**
   * Transforms a rectangle in user space to intermediate space.
   */
  nsIntRect UserSpaceToIntermediateSpace(const gfxRect& aUserSpace,
                                         bool* aOverflow = nullptr) const;

  /**
   * Transforms a rectangle in intermediate space to user space.
   */
  gfxRect IntermediateSpaceToUserSpace(
    const nsIntRect& aIntermediateSpace) const;

  /**
   * Transforms a rectangle in frame space to filter space.
   */
  nsIntRect FrameSpaceToFilterSpace(const nsRect* aFrameSpace) const;

  /**
   * Transforms a rectangle in filter space to frame space.
   */
  nsRect FilterSpaceToFrameSpace(const nsIntRect& aRect) const;

  /**
   * Compute the transformation matrix from user space to
   * frame space in CSS pixels.
   */
  gfxMatrix ComputeUserSpaceToFrameSpaceInCSSPxTransform();

  /**
   * Rect helpers.
   */
  static IntRect ToIntRect(const gfxRect& rect);
  static gfxRect ToGfxRect(const nsIntRect& rect);
  static IntRect InfiniteIntRect();

  /**
   * The frame for the element that is currently being filtered.
   */
  nsIFrame* mTargetFrame;

  nsSVGFilterPaintCallback* mPaintCallback;

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
   * The transforms between filter space and outer-<svg> device space.
   */
  gfxMatrix mFilterSpaceToDeviceSpaceTransform;
  gfxMatrix mDeviceSpaceToFilterSpaceTransform;

  /**
   * The transforms between filter space and frame space, in CSS px. This
   * transform does not transform to frame space in its normal app units, since
   * app units are ints, requiring appropriate rounding which can't be done by
   * a transform matrix. Callers have to do that themselves as appropriate for
   * their needs.
   */
  gfxMatrix mFilterSpaceToFrameSpaceInCSSPxTransform;
  gfxMatrix mFrameSpaceInCSSPxToFilterSpaceTransform;

  /**
   * The bounds of the filtered result, in different spaces.
   */
  gfxRect mUserSpaceBounds;
  nsIntRect mFilterSpaceBounds;

  /**
   * Pre-filter paint bounds of the element that is being filtered, in filter
   * space.
   */
  nsIntRect mTargetBounds;

  /**
   * If set, this is the filter space bounds of the outer-<svg> device space
   * bounds of the dirty area that needs to be repainted. (As bounds-of-bounds,
   * this may be a fair bit bigger than we actually need, unfortunately.)
   */
  nsIntRect mPostFilterDirtyRect;

  /**
   * If set, this is the filter space bounds of the outer-<svg> device bounds
   * of the pre-filter area of the filtered element that changed. (As
   * bounds-of-bounds, this may be a fair bit bigger than we actually need,
   * unfortunately.)
   */
  nsIntRect mPreFilterDirtyRect;

  uint32_t mAppUnitsPerCSSPx;

  SourceInfo mSourceGraphic;
  SourceInfo mFillPaint;
  SourceInfo mStrokePaint;
  nsIFrame* mTransformRoot;
  nsTArray<mozilla::RefPtr<SourceSurface>> mInputImages;
  nsTArray<FilterPrimitiveDescription> mPrimitiveDescriptions;

  nsTArray<nsStyleFilter> mFilters;
  bool mInitialized;
};

#endif
