#pragma once


// CFloatInput dialog

class CFloatInput : public CDialog
{
	DECLARE_DYNAMIC(CFloatInput)

public:
	CFloatInput(CWnd* pParent = NULL);   // standard constructor
	virtual ~CFloatInput();

// Dialog Data
	enum { IDD = IDD_FLOATINPUT };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
public:
	virtual BOOL OnInitDialog();
	double Value;
	char* query;
};

// Float Query
float QueryFloat(char* query, float deflt);