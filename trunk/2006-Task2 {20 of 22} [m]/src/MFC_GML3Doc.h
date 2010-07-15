#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

// MFC_GML3Doc.h : interface of the CImageDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MFC_GML3DOC_H__D2041659_F42C_4A81_9CBD_D3AF4FE3C33E__INCLUDED_)
#define AFX_MFC_GML3DOC_H__D2041659_F42C_4A81_9CBD_D3AF4FE3C33E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "SimpleBitmap.h"

class CImageDoc : public CDocument
{
protected: // create from serialization only
	CImageDoc();
	DECLARE_DYNCREATE(CImageDoc)


// Attributes
public:

  DSimpleBitmap m_Image;

// Operations
public:

  /// Быстрая отрисовка в DC
  void Paint(HDC in_DC);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CImageDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CImageDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CImageDoc)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MFC_GML3DOC_H__D2041659_F42C_4A81_9CBD_D3AF4FE3C33E__INCLUDED_)
