unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, kpPipeline, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Button1: TButton;
    AnimCB: TCheckBox;
    kpPipeline1: TkpPipeline;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    CB0: TCheckBox;
    Label15: TLabel;
    CB1: TCheckBox;
    CB2: TCheckBox;
    Label16: TLabel;
    Label17: TLabel;
    CB3: TCheckBox;
    CB4: TCheckBox;
    Label18: TLabel;
    Label19: TLabel;
    CB5: TCheckBox;
    Label20: TLabel;
    CB6: TCheckBox;
    CB7: TCheckBox;
    Label21: TLabel;
    Label22: TLabel;
    CB8: TCheckBox;
    CB9: TCheckBox;
    Label23: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure AnimCBClick(Sender: TObject);
    procedure kpPipeline1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure kpPipeline1ProcessLine(Sender: TObject; LineNo: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    CurLine: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const FName = 'example.tub';

procedure TForm1.FormCreate(Sender: TObject);
begin
   ChDir(ExtractFileDir(ParamStr(0)));  
try
   Memo1.Lines.LoadFromFile(FName);
except
   Memo1.Lines.Clear;
   Memo1.Lines.Add('s 0 30 s1');
   Memo1.Lines.Add('r 100');
   Memo1.Lines.Add('j j1');
   Memo1.Lines.Add('r 110');
   Memo1.Lines.Add('t t1');
   Memo1.Lines.Add('g j1');
   Memo1.Lines.Add('d 50');
   Memo1.Lines.Add('l 50');
   Memo1.Lines.Add('d 50');
   Memo1.Lines.Add('r 160');
   Memo1.Lines.Add('t t2');
   Memo1.Lines.Add('s 0 200 s2');
   Memo1.Lines.Add('r 100');
   Memo1.Lines.Add('j j2');
   Memo1.Lines.Add('r 110');
   Memo1.Lines.Add('t t4');
   Memo1.Lines.Add('g j2');
   Memo1.Lines.Add('u 100');
   Memo1.Lines.Add('r 40');
   Memo1.Lines.Add('u 30');
   Memo1.Lines.Add('r 70');
   Memo1.Lines.Add('t t3');
end;
   Button1Click(Self);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Memo1.Lines.SaveToFile(FName);
end;

procedure TForm1.Button1Click(Sender: TObject);
var i, count: integer;
    C: TCheckBox;
begin
try try
    kpPipeline1.MapList := Memo1.Lines;
except
    Count := 0;
    for i:=0 to CurLine-1 do
       Inc(Count, Length(Memo1.Lines[i])+2);
    Memo1.SelStart := count;
    Memo1.SelLength := Length(Memo1.Lines[CurLine]);
    if Visible then Memo1.SetFocus;
    raise;
end;
finally
    for i:=0 to 9 do begin
       C := TCheckBox(FindComponent('CheckBox' + IntToStr(i+1)));
       C.Checked := False;
    end;
end;
end;

procedure TForm1.AnimCBClick(Sender: TObject);
begin
   kpPipeline1.Animate := AnimCB.Checked;
end;

procedure TForm1.kpPipeline1Change(Sender: TObject);
var i: integer;
    C: TCheckBox;
begin
   for i:=0 to 9 do begin
      C := TCheckBox(FindComponent('CB' + IntToStr(i)));
      if i < kpPipeline1.TerminalNames.Count then
           C.Checked := kpPipeline1.TerminalState[i]
      else C.Checked := False;
   end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var C: TCheckBox;
begin
   C := Sender as TCheckBox;
   kpPipeline1.SourceState[C.Tag] := C.Checked;
end;

procedure TForm1.kpPipeline1ProcessLine(Sender: TObject; LineNo: Integer);
begin
   CurLine := LineNo;
end;

end.
