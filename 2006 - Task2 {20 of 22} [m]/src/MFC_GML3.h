#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

// MFC_GML3.h : main header file for the MFC_GML3 application
//

#if !defined(AFX_MFC_GML3_H__D296EFDD_780E_4AF6_B7F2_48B81EDD8ABC__INCLUDED_)
#define AFX_MFC_GML3_H__D296EFDD_780E_4AF6_B7F2_48B81EDD8ABC__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CApplication:
// See MFC_GML3.cpp for the implementation of this class
//

class CApplication : public CWinApp
{
public:
	CApplication();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CApplication)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation
	//{{AFX_MSG(CApplication)
	//afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MFC_GML3_H__D296EFDD_780E_4AF6_B7F2_48B81EDD8ABC__INCLUDED_)
