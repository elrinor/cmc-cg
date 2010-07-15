// FloatInput.cpp : implementation file
//

#include "stdafx.h"
#include "MFC_GML3.h"
#include "FloatInput.h"


// CFloatInput dialog

IMPLEMENT_DYNAMIC(CFloatInput, CDialog)
CFloatInput::CFloatInput(CWnd* pParent /*=NULL*/)
	: CDialog(CFloatInput::IDD, pParent)
	, Value(0)
{
}

CFloatInput::~CFloatInput()
{
}

void CFloatInput::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	DDX_Text(pDX, IDC_EDIT1, Value);
}

BOOL CFloatInput::OnInitDialog() 
{
	CDialog::OnInitDialog();
	this->GetDlgItem(IDC_QUERY)->SetWindowText(query);
	return true;
}

BEGIN_MESSAGE_MAP(CFloatInput, CDialog)
END_MESSAGE_MAP()

float QueryFloat(char* query, float deflt)
{
	CFloatInput floatInput;
	floatInput.query=query;
	if (floatInput.DoModal()==IDOK)
		return (float)floatInput.Value;
	else
		return deflt;
}


// CFloatInput message handlers
