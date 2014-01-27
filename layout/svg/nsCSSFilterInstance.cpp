/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// Main header first:
#include "nsCSSFilterInstance.h"

// Keep others in (case-insensitive) order:
#include "FilterSupport.h"

using namespace mozilla;
using namespace mozilla::dom;
using namespace mozilla::gfx;

IntRect
nsCSSFilterInstance::InfiniteIntRect()
{
  return IntRect(-INT_MAX / 2, -INT_MAX / 2, INT_MAX, INT_MAX);
}

nsCSSFilterInstance::nsCSSFilterInstance(
  const nsStyleFilter& aFilter,
  nsTArray<FilterPrimitiveDescription>& aPrimitiveDescriptions) :
    mFilter(aFilter),
    mPrimitiveDescriptions(aPrimitiveDescriptions),
    mInitialized(false)
{
  nsresult rv = BuildPrimitives();
  if (NS_FAILED(rv)) {
    return;
  }

  mInitialized = true;
}

nsresult
nsCSSFilterInstance::BuildPrimitives()
{
  nsresult result;
  switch(mFilter.GetType()) {
    case NS_STYLE_FILTER_BLUR:
      result = BuildPrimitivesForBlur();
      break;
    default:
      NS_NOTREACHED("not a CSS filter type");
      result = NS_ERROR_FAILURE;
      break;
  }
  return result;  
}

static const float kMaxStdDeviation = 500;

nsresult
nsCSSFilterInstance::BuildPrimitivesForBlur()
{
  FilterPrimitiveDescription descr(FilterPrimitiveDescription::eGaussianBlur);
  descr.SetPrimitiveSubregion(InfiniteIntRect());

  uint32_t numPrimitiveDescriptions = mPrimitiveDescriptions.Length();
  if (numPrimitiveDescriptions > 0) {
    // Use the output of the last filter primitive description as the input.
    uint32_t lastPrimitiveDescrIndex = numPrimitiveDescriptions - 1;
    descr.SetInputPrimitive(0, lastPrimitiveDescrIndex);

    ColorSpace lastColorSpace =
      mPrimitiveDescriptions[lastPrimitiveDescrIndex].OutputColorSpace();
    descr.SetInputColorSpace(0, lastColorSpace);
    descr.SetOutputColorSpace(lastColorSpace);
  } else {
    // Use the SourceGraphic as the input.
    descr.SetInputPrimitive(0,
      FilterPrimitiveDescription::kPrimitiveIndexSourceGraphic);
    descr.SetInputColorSpace(0, SRGB);
    descr.SetOutputColorSpace(SRGB);
  }

  nsStyleCoord radiusStyleCoord = mFilter.GetFilterParameter();
  if (radiusStyleCoord.GetUnit() != eStyleUnit_Coord) {
    NS_NOTREACHED("unexpected unit");
    return NS_ERROR_FAILURE;
  }

  nscoord radiusCoord = radiusStyleCoord.GetCoordValue();
  float radius = nsPresContext::AppUnitsToFloatCSSPixels(radiusCoord);
  if (radius < 0) {
    NS_NOTREACHED("we shouldn't have parsed a negative value");
    return NS_ERROR_FAILURE;
  }

  radius = std::min(radius, kMaxStdDeviation);
  descr.Attributes().Set(eGaussianBlurStdDeviation, Size(radius, radius));

  mPrimitiveDescriptions.AppendElement(descr);
  return NS_OK;
}
