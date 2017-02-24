object Form2: TForm2
  Left = 287
  Top = 382
  BorderStyle = bsDialog
  Caption = 'Nastaven'#237
  ClientHeight = 184
  ClientWidth = 188
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 152
    Width = 81
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 152
    Width = 89
    Height = 25
    Caption = 'zru'#353'it'
    TabOrder = 1
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 169
    Height = 17
    Caption = 'zv'#253'raz'#328'ovat tahy'
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 177
    Height = 105
    Caption = 'vzhled'
    TabOrder = 3
    object Image1: TImage
      Left = 8
      Top = 48
      Width = 52
      Height = 52
    end
    object Image2: TImage
      Left = 64
      Top = 48
      Width = 52
      Height = 52
    end
    object Image3: TImage
      Left = 120
      Top = 48
      Width = 52
      Height = 52
    end
    object RadioButton1: TRadioButton
      Left = 24
      Top = 24
      Width = 33
      Height = 17
      Caption = '1'
      TabOrder = 0
    end
    object RadioButton2: TRadioButton
      Left = 72
      Top = 24
      Width = 33
      Height = 17
      Caption = '2'
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 128
      Top = 24
      Width = 33
      Height = 17
      Caption = '3'
      TabOrder = 2
    end
  end
end
