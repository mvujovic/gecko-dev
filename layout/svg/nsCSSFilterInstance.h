/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef __NS_CSSFILTERINSTANCE_H__
#define __NS_CSSFILTERINSTANCE_H__

#include "nsTArray.h"
#include "nsIFrame.h"

class nsIFrame;

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
