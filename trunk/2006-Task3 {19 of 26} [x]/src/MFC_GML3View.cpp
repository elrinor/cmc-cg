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
	ON_COMMAND(ID_MEDIAN_NORMAL, OnMedianNormal)
  ON_COMMAND(ID_RECOGNITION_CONTRAST, OnRecognitionContrast)
  ON_COMMAND(ID_RECOGNITION_BINARIZATION, OnRecognitionBinarization)
  ON_COMMAND(ID_RECOGNITION_DILATION, OnRecognitionDilation)
  ON_COMMAND(ID_RECOGNITION_EROSION, OnRecognitionErosion)
  ON_COMMAND(ID_RECOGNITION_BINARYCLOSE, OnRecognitionBinaryclose)
  ON_COMMAND(ID_RECOGNITION_BINARYOPEN, OnRecognitionBinaryopen)
  ON_COMMAND(ID_RECOGNITION_SIMPLEPROCESSING, OnRecognitionSimpleprocessing)
  ON_COMMAND(ID_RECOGNITION_REMOVENOISE, OnRecognitionRemovenoise)
  ON_COMMAND(ID_RECOGNITION_RECOGNITION, OnRecognitionRecognition)
  ON_COMMAND(ID_FULLCYCLE_MEDIUM3PROCESSING, OnFullcycleMedium3processing)
  ON_COMMAND(ID_FULLCYCLE_MEDIUM12PROCESSING, OnFullcycleMedium12processing)
  ON_COMMAND(ID_FULLCYCLE_MEDIUM5PROCESSING, OnFullcycleMedium5processing)
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

void CImageView::OnMedianNormal()
{
	CImageDoc* pDoc=GetDocument();
	MedianNormal(&pDoc->m_Image, (int)QueryFloat("Enter Median Radius.\nRecommended Values: 1-10\nExample: 2", 0));
  Invalidate();
}

void CImageView::OnRecognitionContrast()
{
	CImageDoc* pDoc=GetDocument();
	Contrast(&pDoc->m_Image);
  Invalidate();
}

void CImageView::OnRecognitionBinarization()
{
	CImageDoc* pDoc=GetDocument();
  Binarization(&pDoc->m_Image);
  Invalidate();
}

void CImageView::OnRecognitionDilation()
{
	CImageDoc* pDoc=GetDocument();
	BinaryDilation(&pDoc->m_Image, (int)QueryFloat("Enter Dilation Radius.\nRecommended Values: 1-50\nExample: 10", 0));
  Invalidate();
}

void CImageView::OnRecognitionErosion()
{
	CImageDoc* pDoc=GetDocument();
	BinaryErosion(&pDoc->m_Image, (int)QueryFloat("Enter Erosion Radius.\nRecommended Values: 1-50\nExample: 10", 0));
  Invalidate();
}

void CImageView::OnRecognitionBinaryclose()
{
	CImageDoc* pDoc=GetDocument();
	BinaryClose(&pDoc->m_Image, (int)QueryFloat("Enter Closing Radius.\nRecommended Values: 1-50\nExample: 10", 0));
  Invalidate();
}

void CImageView::OnRecognitionBinaryopen()
{
	CImageDoc* pDoc=GetDocument();
	BinaryOpen(&pDoc->m_Image, (int)QueryFloat("Enter Opening Radius.\nRecommended Values: 1-50\nExample: 10", 0));
  Invalidate();
}

void CImageView::OnRecognitionSimpleprocessing()
{
  string s;
	CImageDoc* pDoc=GetDocument();
  DSimpleBitmap Src;
  Src=pDoc->m_SrcImage;
	MedianNormal(&pDoc->m_Image, 1);
  Binarization(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
  BinaryClose(&pDoc->m_Image, 3);
	MedianNormal(&pDoc->m_Image, 1);
  Scan(&Src, &pDoc->m_Image, &s, 1200);
  pDoc->m_Image=Src;
  Invalidate();
  MessageBox(s.c_str());
}

void CImageView::OnRecognitionRemovenoise()
{
	CImageDoc* pDoc=GetDocument();
	RemovePoints(&pDoc->m_Image);
  Invalidate();
}

void CImageView::OnRecognitionRecognition()
{
  string s;
	CImageDoc* pDoc=GetDocument();
  DSimpleBitmap Src;
  Src=pDoc->m_SrcImage;
  Scan(&Src, &pDoc->m_Image, &s, 300);
  pDoc->m_Image=Src;
  Invalidate();
  MessageBox(s.c_str());
}

void CImageView::OnFullcycleMedium3processing()
{
  string s;
	CImageDoc* pDoc=GetDocument();
  DSimpleBitmap Src;
  Src=pDoc->m_SrcImage;
  Binarization(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	BinaryErosion(&pDoc->m_Image, 3);
  BinaryDilation(&pDoc->m_Image, 6);
  BinaryErosion(&pDoc->m_Image, 2);
  RemovePoints(&pDoc->m_Image);
  Scan(&Src, &pDoc->m_Image, &s, 1000);
  pDoc->m_Image=Src;
  Invalidate();
  MessageBox(s.c_str());
}

void CImageView::OnFullcycleMedium12processing()
{
  string s;
	CImageDoc* pDoc=GetDocument();
  DSimpleBitmap Src;
  Src=pDoc->m_SrcImage;
  Contrast(&Src);
  pDoc->m_Image=Src;
  Binarization(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
  RemovePoints(&pDoc->m_Image);
  RemovePoints(&pDoc->m_Image);
  RemovePoints(&pDoc->m_Image);
  RemovePoints(&pDoc->m_Image);
  Scan(&Src, &pDoc->m_Image, &s, 400);
  pDoc->m_Image=Src;
  Invalidate();
  MessageBox(s.c_str());
}

void CImageView::OnFullcycleMedium5processing()
{
  string s;
	CImageDoc* pDoc=GetDocument();
  DSimpleBitmap Src;
  Src=pDoc->m_SrcImage;
  Contrast(&pDoc->m_Image);
  Contrast(&pDoc->m_Image);
  Contrast(&pDoc->m_Image);
  Contrast(&pDoc->m_Image);
  Contrast(&pDoc->m_Image);
  Contrast(&pDoc->m_Image);
  Contrast(&pDoc->m_Image);
  Binarization(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	BinaryErosion(&pDoc->m_Image, 2);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
	RemovePoints(&pDoc->m_Image);
  BinaryDilation(&pDoc->m_Image, 10);
  BinaryErosion(&pDoc->m_Image, 4);
  Scan(&Src, &pDoc->m_Image, &s, 300);
  pDoc->m_Image=Src;
  Invalidate();
  MessageBox(s.c_str());
}
