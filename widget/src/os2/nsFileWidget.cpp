/* -*- Mode: c++; tab-width: 2; indent-tabs-mode: nil; -*- */
/*
 * The contents of this file are subject to the Netscape Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/NPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is mozilla.org code.
 *
 * The Initial Developer of the Original Code is Netscape
 * Communications Corporation.  Portions created by Netscape are
 * Copyright (C) 1998 Netscape Communications Corporation. All
 * Rights Reserved.
 *
 * Contributor(s): 
 *   Pierre Phaneuf <pp@ludusdesign.com>
 */

// Define so header files for openfilename are included
#ifdef WIN32_LEAN_AND_MEAN
#undef WIN32_LEAN_AND_MEAN
#endif

#include "nsCOMPtr.h"
#include "nsIServiceManager.h"
#define NS_IMPL_IDS
#include "nsIPlatformCharset.h"
#undef NS_IMPL_IDS
#include "nsWidgetDefs.h"
#include "nsDirPicker.h"
#include "nsFileWidget.h"
#include "nsFileSpec.h"


static NS_DEFINE_CID(kCharsetConverterManagerCID, NS_ICHARSETCONVERTERMANAGER_CID);

NS_IMPL_ADDREF(nsFileWidget)
NS_IMPL_RELEASE(nsFileWidget)


//-------------------------------------------------------------------------
//
// nsFileWidget constructor
//
//-------------------------------------------------------------------------
nsFileWidget::nsFileWidget() : nsIFileWidget()
{
  NS_INIT_REFCNT();
  mWnd = NULL;
  mNumberOfFilters = 0;
  mUnicodeEncoder = nsnull;
  mUnicodeDecoder = nsnull;
}

//-------------------------------------------------------------------------
//
// nsFileWidget destructor
//
//-------------------------------------------------------------------------
nsFileWidget::~nsFileWidget()
{
  NS_IF_RELEASE(mUnicodeEncoder);
  NS_IF_RELEASE(mUnicodeDecoder);
}

/**
 * @param aIID The name of the class implementing the method
 * @param _classiiddef The name of the #define symbol that defines the IID
 * for the class (e.g. NS_ISUPPORTS_IID)
 * 
*/ 
nsresult nsFileWidget::QueryInterface(const nsIID& aIID, void** aInstancePtr)
{

  if (NULL == aInstancePtr) {
    return NS_ERROR_NULL_POINTER;
  }

  nsresult rv = NS_NOINTERFACE;

  if (aIID.Equals(NS_GET_IID(nsIFileWidget))) {
    *aInstancePtr = (void*) ((nsIFileWidget*)this);
    NS_ADDREF_THIS();
    return NS_OK;
  }

  return rv;
}


//-------------------------------------------------------------------------
//
// Show - Display the file dialog
//
//-------------------------------------------------------------------------

PRBool nsFileWidget::Show()
{
  char fileBuffer[MAX_PATH+1] = "";
  char *converted = ConvertToFileSystemCharset(mDefault.get());
  if (nsnull == converted) {
    mDefault.ToCString(fileBuffer,MAX_PATH);
  }
  else {
    PL_strcpy(fileBuffer, converted);
    delete [] converted;
  }

  OPENFILENAME ofn;
  memset(&ofn, 0, sizeof(ofn));

  ofn.lStructSize = sizeof(ofn);

  nsString filterList;
  GetFilterListArray(filterList);
  char *filterBuffer = ConvertToFileSystemCharset(filterList.get(), filterList.Length());
  char *title = ConvertToFileSystemCharset(mTitle.get());
  if (nsnull == title)
    title = mTitle.ToNewCString();
  const char *initialDir = mDisplayDirectory.GetNativePathCString();
  if (initialDir && *initialDir) {
     ofn.lpstrInitialDir = initialDir;
  }

  ofn.lpstrTitle   = title;
  ofn.lpstrFilter  = filterBuffer;
  ofn.nFilterIndex = 1;
  ofn.hwndOwner    = mWnd;
  ofn.lpstrFile    = fileBuffer;
  ofn.nMaxFile     = MAX_PATH;

  // XXX use OFN_NOCHANGEDIR  for M5
  ofn.Flags = OFN_SHAREAWARE | OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY;
  
  PRBool result = PR_TRUE;
  LONG retval;

  if (mMode == eMode_load) {
    // FILE MUST EXIST!
    ofn.Flags |= OFN_FILEMUSTEXIST;
  }
  if( !DaxOpenSave( (mMode == eMode_save), &retval, &ofn, NULL) )
    result = PR_FALSE;
  
  // Remember what filter type the user selected
  mSelectedType = (PRInt16)ofn.nFilterIndex;

   // Store the current directory in mDisplayDirectory
  char* newCurrentDirectory = new char[MAX_PATH+1];
  VERIFY(gWidgetModuleData->GetCurrentDirectory(MAX_PATH, newCurrentDirectory) > 0);
  mDisplayDirectory = newCurrentDirectory;
  delete[] newCurrentDirectory;

   // Clean up filter buffers
  if (filterBuffer)
    delete[] filterBuffer;
  if (title)
    delete[] title;

   // Set user-selected location of file or directory
  mFile.SetLength(0);
  if (result == PR_TRUE) {
    // I think it also needs a conversion here (to unicode since appending to nsString) 
    // but doing that generates garbage file name, weird.
    mFile.AppendWithConversion(fileBuffer);
  }
  
  return((PRBool)result);
}

//-------------------------------------------------------------------------
//
// Convert filter titles + filters into a Windows filter string
//
//-------------------------------------------------------------------------

void nsFileWidget::GetFilterListArray(nsString& aFilterList)
{
  aFilterList.SetLength(0);
  for (PRUint32 i = 0; i < mNumberOfFilters; i++) {
    const nsString& title = mTitles[i];
    const nsString& filter = mFilters[i];
    
    aFilterList.Append(title);
    aFilterList.AppendWithConversion('\0');
    aFilterList.Append(filter);
    aFilterList.AppendWithConversion('\0');
  }
  aFilterList.AppendWithConversion('\0'); 
}

//-------------------------------------------------------------------------
//
// Set the list of filters
//
//-------------------------------------------------------------------------

NS_IMETHODIMP nsFileWidget::SetFilterList(PRUint32 aNumberOfFilters,const nsString aTitles[],const nsString aFilters[])
{
  mNumberOfFilters  = aNumberOfFilters;
  mTitles           = aTitles;
  mFilters          = aFilters;
  return NS_OK;
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
NS_IMETHODIMP  nsFileWidget::GetSelectedType(PRInt16& theType)
{
  theType = mSelectedType;
  return NS_OK;
}

//-------------------------------------------------------------------------
//
//-------------------------------------------------------------------------
NS_IMETHODIMP  nsFileWidget::GetFile(nsFileSpec& aFile)
{
  nsFilePath filePath(mFile);
  nsFileSpec fileSpec(filePath);

  aFile = filePath;
  return NS_OK;
}


//-------------------------------------------------------------------------
//
// Get the file + path
//
//-------------------------------------------------------------------------
NS_IMETHODIMP  nsFileWidget::SetDefaultString(const nsString& aString)
{
  mDefault = aString;
  return NS_OK;
}


//-------------------------------------------------------------------------
//
// Set the display directory
//
//-------------------------------------------------------------------------
NS_IMETHODIMP  nsFileWidget::SetDisplayDirectory(const nsFileSpec& aDirectory)
{
  mDisplayDirectory = aDirectory;
  return NS_OK;
}


//-------------------------------------------------------------------------
//
// Get the display directory
//
//-------------------------------------------------------------------------
NS_IMETHODIMP  nsFileWidget::GetDisplayDirectory(nsFileSpec& aDirectory)
{
  aDirectory = mDisplayDirectory;
  return NS_OK;
}


//-------------------------------------------------------------------------
NS_IMETHODIMP nsFileWidget::Create(nsIWidget *aParent,
                                   const nsString& aTitle,
                                   nsFileDlgMode aMode,
                                   nsIDeviceContext *aContext,
                                   nsIAppShell *aAppShell,
                                   nsIToolkit *aToolkit,
                                   void *aInitData)
{
  mWnd = (HWND) ((aParent) ? aParent->GetNativeData(NS_NATIVE_WINDOW) : 0); 
  mTitle.SetLength(0);
  mTitle.Append(aTitle);
  mMode = aMode;
  return NS_OK;
}

//-------------------------------------------------------------------------
nsFileDlgResults nsFileWidget::GetFile(nsIWidget        * aParent,
                                       const nsString   & promptString,    
                                       nsFileSpec       & theFileSpec)
{
  Create(aParent, promptString, eMode_load, nsnull, nsnull);
  PRBool result = Show();
  nsFileDlgResults status = nsFileDlgResults_Cancel;
  if (result && mFile.Length() > 0) {
    nsFilePath filePath(mFile);
    nsFileSpec fileSpec(filePath);
    theFileSpec = fileSpec;
    status = nsFileDlgResults_OK;
  }
  return status;
}

//-------------------------------------------------------------------------
nsFileDlgResults nsFileWidget::GetFolder(nsIWidget        * aParent,
                                         const nsString   & promptString,    
                                         nsFileSpec       & theFileSpec)
{

   nsFileDlgResults status = nsFileDlgResults_Cancel;

   HWND hwndOwner = HWND_DESKTOP;
   if( aParent)
      hwndOwner = (HWND) aParent->GetNativeData( NS_NATIVE_WIDGET);

   DIRPICKER dp = { { 0 }, 0, TRUE, 0 }; // modal dialog

   gWidgetModuleData->ConvertFromUcs( promptString, dp.szFullFile, CCHMAXPATH);

   HWND ret = FS_PickDirectory( HWND_DESKTOP, hwndOwner,
                                gWidgetModuleData->hModResources, &dp);

   if( ret && dp.lReturn == DID_OK)
   {
      theFileSpec = dp.szFullFile;
      status = nsFileDlgResults_OK;
   }

   return status;
}

//-------------------------------------------------------------------------
nsFileDlgResults nsFileWidget::PutFile(nsIWidget        * aParent,
                                       const nsString   & promptString,    
                                       nsFileSpec       & theFileSpec)
{
  Create(aParent, promptString, eMode_save, nsnull, nsnull);
  PRBool result = Show();
  nsFileDlgResults status = nsFileDlgResults_Cancel;
  if (result && mFile.Length() > 0) {
    nsFilePath filePath(mFile);
    nsFileSpec fileSpec(filePath);
    theFileSpec = fileSpec;

    if (result) {
      status = (theFileSpec.Exists()?nsFileDlgResults_Replace:nsFileDlgResults_OK);
    }
  }
  return status;
}

//-------------------------------------------------------------------------
void nsFileWidget::GetFileSystemCharset(nsString & fileSystemCharset)
{
  static nsAutoString aCharset;
  nsresult rv;

  if (aCharset.Length() < 1) {
    nsCOMPtr <nsIPlatformCharset> platformCharset = do_GetService(NS_PLATFORMCHARSET_CONTRACTID, &rv);
	  if (NS_SUCCEEDED(rv)) 
		  rv = platformCharset->GetCharset(kPlatformCharsetSel_FileName, aCharset);

    NS_ASSERTION(NS_SUCCEEDED(rv), "error getting platform charset");
	  if (NS_FAILED(rv)) 
		  aCharset.AssignWithConversion("windows-1252");
  }
  fileSystemCharset = aCharset;
}

//-------------------------------------------------------------------------
char * nsFileWidget::ConvertToFileSystemCharset(const PRUnichar *inString, PRInt32 inLength)
{
  char *outString = nsnull;
  nsresult rv = NS_OK;

  // get file system charset and create a unicode encoder
  if (nsnull == mUnicodeEncoder) {
    nsAutoString fileSystemCharset;
    GetFileSystemCharset(fileSystemCharset);

    nsCOMPtr<nsICharsetConverterManager> ccm = 
             do_GetService(kCharsetConverterManagerCID, &rv); 
    if (NS_SUCCEEDED(rv)) {
      rv = ccm->GetUnicodeEncoder(&fileSystemCharset, &mUnicodeEncoder);
    }
  }

  // converts from unicode to the file system charset
  if (NS_SUCCEEDED(rv)) {
    if(inLength < 0)
		inLength = nsCRT::strlen(inString);
    PRInt32 outLength;
    rv = mUnicodeEncoder->GetMaxLength(inString, inLength, &outLength);
    if (NS_SUCCEEDED(rv)) {
      outString = new char[outLength+1];
      if (nsnull == outString) {
        return nsnull;
      }
      rv = mUnicodeEncoder->Convert(inString, &inLength, outString, &outLength);
      if (NS_SUCCEEDED(rv)) {
        outString[outLength] = '\0';
      }
    }
  }
  
  return NS_SUCCEEDED(rv) ? outString : nsnull;
}

