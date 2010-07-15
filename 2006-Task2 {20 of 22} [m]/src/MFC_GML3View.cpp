#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

// MFC_GML3View.cpp : implementation of the CImageView class
//

#include "stdafx.h"
#include "MFC_GML3.h"

#include "MFC_GML3Doc.h"
#include "MFC_GML3View.h"
#include "Processing.h"
#include "FloatInput.h"
#include ".\mfc_gml3view.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CImageView

IMPLEMENT_DYNCREATE(CImageView, CView)

BEGIN_MESSAGE_MAP(CImageView, CView)
	ON_COMMAND(ID_NOISE_WHITE, OnNoiseWhite)
	ON_COMMAND(ID_NOISE_IMPULSE, OnNoiseImpulse)
	ON_COMMAND(ID_FILTERS3X3_SHARPEN, OnFilters3x3Sharpen)
	ON_COMMAND(ID_FILTERS3X3_BLUR, OnFilters3x3Blur)
	ON_COMMAND(ID_FILTERS3X3_FINDEDGES, OnFilters3x3Findedges)
	ON_COMMAND(ID_GAUSSIANBLUR_1DVERSION, OnGaussianblur1dversion)
	ON_COMMAND(ID_GAUSSIANBLUR_2DVERSION, OnGaussianblur2dversion)
	ON_COMMAND(ID_MEDIAN_NORMAL, OnMedianNormal)
	ON_COMMAND(ID_MEDIAN_VECTOR, OnMedianVector)
	ON_COMMAND(ID_KNN_MANUAL, OnKnnManual)
	ON_COMMAND(ID_NLM_MANUAL, OnNlmManual)
	ON_COMMAND(ID_KNN_AUTOMATIC, OnKnnAutomatic)
	ON_COMMAND(ID_NLM_AUTOMATIC, OnNlmAutomatic)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CImageView construction/destruction

CImageView::CImageView()
{
	// TODO: add construction code here

}

CImageView::~CImageView()
{
}

BOOL CImageView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CImageView drawing

void CImageView::OnDraw(CDC* pDC)
{
	CImageDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

  /// Требуется отрисовать наше изображение в присланный device context
  pDoc->Paint(pDC->m_hDC);
}

/////////////////////////////////////////////////////////////////////////////
// CImageView diagnostics

#ifdef _DEBUG
void CImageView::AssertValid() const
{
	CView::AssertValid();
}

void CImageView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CImageDoc* CImageView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CImageDoc)));
	return (CImageDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CImageView message handlers

unsigned char pcBuf[1024 * 1024];

void CImageView::OnNoiseWhite() 
{
  CImageDoc* pDoc=GetDocument();
	WhiteNoise(&pDoc->m_Image, (int)QueryFloat("Enter Noise Amplitude.\nRecommended Values: 1-300\nExample: 50", 0));
  Invalidate();
}

void CImageView::OnNoiseImpulse()
{
  CImageDoc* pDoc=GetDocument();
	ImpulseNoise(&pDoc->m_Image, (int)QueryFloat("Enter Noise Probability.\nRecommended Values: 1-100\nExample: 20", 0));
  Invalidate();
}

void CImageView::OnFilters3x3Sharpen()
{
  CImageDoc* pDoc=GetDocument();
	Sharpen(&pDoc->m_Image);
  Invalidate();
}

void CImageView::OnFilters3x3Blur()
{
  CImageDoc* pDoc=GetDocument();
	Blur(&pDoc->m_Image);
  Invalidate();
}

void CImageView::OnFilters3x3Findedges()
{
  CImageDoc* pDoc=GetDocument();
	FindEdges(&pDoc->m_Image);
  Invalidate();
}

void CImageView::OnGaussianblur1dversion()
{
  CImageDoc* pDoc=GetDocument();
	GaussianBlur1D(&pDoc->m_Image, QueryFloat("Enter Gaussian Blur Power.\nRecommended Values: 0-50\nExample: 1.5", 0));
  Invalidate();
}

void CImageView::OnGaussianblur2dversion()
{
  CImageDoc* pDoc=GetDocument();
	GaussianBlur2D(&pDoc->m_Image, QueryFloat("Enter Gaussian Blur Power.\nRecommended Values: 0-50\nExample: 1.5", 0));
  Invalidate();
}

void CImageView::OnMedianNormal()
{
	CImageDoc* pDoc=GetDocument();
	MedianNormal(&pDoc->m_Image, (int)QueryFloat("Enter Median Radius.\nRecommended Values: 1-10\nExample: 2", 0));
  Invalidate();
}

void CImageView::OnMedianVector()
{
	CImageDoc* pDoc=GetDocument();
	MedianVector(&pDoc->m_Image, (int)QueryFloat("Enter Median Radius.\nRecommended Values: 1-10\nExample: 2", 0));
  Invalidate();
}

void CImageView::OnKnnManual()
{
	CImageDoc* pDoc=GetDocument();
	KNN(&pDoc->m_Image, (int)QueryFloat("Enter KNN Radius.\nRecommended Values: 1-10\nExample: 2", 0),
		                       QueryFloat("Enter KNN Power.\nRecommended Values: 25-50\nExample: 27.5", 0));
  Invalidate();
}


void CImageView::OnNlmManual()
{
	CImageDoc* pDoc=GetDocument();
	NLM(&pDoc->m_Image, (int)QueryFloat("Enter NLM Radius.\nRecommended Values: 1-10\nExample: 2", 0),
		                       QueryFloat("Enter NLM Power.\nRecommended Values: 8-30\nExample: 12.5", 0));
  Invalidate();
}

void CImageView::OnKnnAutomatic()
{
	CImageDoc* pDoc=GetDocument();
	KNNAutomatic(&pDoc->m_Image);
  Invalidate();
}

void CImageView::OnNlmAutomatic()
{
	CImageDoc* pDoc=GetDocument();
	NLMAutomatic(&pDoc->m_Image);
  Invalidate();
}
