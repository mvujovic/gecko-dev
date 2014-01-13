/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef __NS_CSSFILTERINSTANCE_H__
#define __NS_CSSFILTERINSTANCE_H__

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
class gfxImageSurface;
class nsIFrame;
class nsSVGFilterPaintCallback;

namespace mozilla {
namespace dom {
class SVGFilterElement;
}
}

class nsCSSFilterInstance
{
typedef mozilla::gfx::FilterPrimitiveDescription FilterPrimitiveDescription;

public:
  nsCSSFilterInstance(
    const nsStyleFilter& aFilter,
    nsTArray<FilterPrimitiveDescription>& aPrimitiveDescriptions);

  bool IsInitialized() const { return mInitialized; }

private:
  nsresult BuildPrimitives();
  nsresult BuildPrimitivesForBlur();

  static IntRect InfiniteIntRect();

  nsStyleFilter mFilter;
  nsTArray<FilterPrimitiveDescription>& mPrimitiveDescriptions;
  bool mInitialized;
};

#endif
