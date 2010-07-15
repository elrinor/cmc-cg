#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

// MFC_GML3Doc.cpp : implementation of the CImageDoc class
//

#include "stdafx.h"
#include "MFC_GML3.h"

#include "MFC_GML3Doc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
/////////////////////////////////////////////////////////////////////////////
// CImageDoc

IMPLEMENT_DYNCREATE(CImageDoc, CDocument)

BEGIN_MESSAGE_MAP(CImageDoc, CDocument)
	//{{AFX_MSG_MAP(CImageDoc)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CImageDoc construction/destruction

CImageDoc::CImageDoc()
{
}


//===================================================================
//= Function name	: CImageDoc::~CImageDoc
//= Description   : 
//= Return type   : 
//===================================================================

CImageDoc::~CImageDoc()
{
}


//===================================================================
//= Function name	: CImageDoc::OnNewDocument
//= Description   : 
//= Return type   : BOOL 
//===================================================================

BOOL CImageDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

  // Создадим пустое изображения 100x100
  m_Image.CreateImage(100, 100);
  m_SrcImage.CreateImage(100, 100);

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CImageDoc diagnostics

#ifdef _DEBUG
void CImageDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CImageDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CImageDoc commands


//===================================================================
//= Function name	: CImageDoc::OnOpenDocument
//= Description   : Обработка события загрузки документа
//= Return type   : BOOL 
//===================================================================

BOOL CImageDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
  /// Вызвали загрузку	
	return m_Image.LoadBitmap(lpszPathName) && m_SrcImage.LoadBitmap(lpszPathName);;
}


//===================================================================
//= Function name	: CImageDoc::OnSaveDocument
//= Description   : Обработка события сохранения документа
//= Return type   : BOOL 
//===================================================================

BOOL CImageDoc::OnSaveDocument(LPCTSTR lpszPathName) 
{
  /// Вызвали сохранение	
	return m_Image.SaveBitmap(lpszPathName);
}


//===================================================================
//= Function name	: CImageDoc::Paint
//= Description   : Быстрая отрисовка в device context
//= Return type   : void 
//===================================================================

void CImageDoc::Paint(HDC in_hDC)
{
  /// Вызвали сохранение	
	m_Image.Paint(in_hDC);
}


/////////////////////////////////////////////////////////////////////////////
// CImageDoc serialization

void CImageDoc::Serialize(CArchive& ar)
{
  /// Не будем мучаться с поддержкой CArchive, поступим проще
}

