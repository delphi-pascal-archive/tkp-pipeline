unit kpPipeline;
{
     TkpPipeline - трубопровод с анимацией

     версия 1.0
     (c) К. Поляков, 2003

     FIDO:   2:5030/542.251
     e-mail: kpolyakov@mail.ru
     Web:    http://kpolyakov.narod.ru
             http://kpolyakov.newmail.ru

     Ограничения:
        - одностороннее движение жидкости
        - ветки могут разветвляться, но не могут пересекаться  
        - трубы соединяются только под прямым углом
        - движение потока имитируется движением прямоугольников
          разных цветов 

     Описание схемы представляет собой набор символьных
     строк. Каждая строка описывает один элемент:
        s x y имя_входа    вход в точке (x,y)
        l длина            звено влево
        г длина            звено вправо
        u длина            звено вверх
        d длина            звено вниз
        j имя_узла         новый узел в текущей позиции курсора
        t имя_выхода       выход в текущей позиции курсора
        g имя_узла         перейти в узел

     Дополнительные свойства и методы:
        Animate: Boolean     использовать анимацию или нет
        Color: TColor        цвет воды
        FlowColor: TColor    цвет промежутков
        AltColor: TColor     цвет промежутков
        BkColor: TColor      цвет фона
        BkBitmap: TBitmap    фоновый рисунок
        JointColor: TColor   цвет контура узлов
        TickLength: integer  длина отрезка при анимации
        PipeWidth: integer   ширина трубы
        MapList: TStrings    описание трубопровода
        Period: integer      интервал для анимации
        MoveStep: integer    шаг движения
        HasLabels: Boolean   ставить ли метки входов и выходов
        SourceState[Index: integer]: Boolean       состояние входа (да/нет)
        TerminalState[Index: integer]: Boolean     состояние выхода (да/нет)
        SourceStateByName[Name: string]: Boolean   состояние входа по имени
        TerminalStateByName[Name: string]: Boolean состояние выхода по имени
        SourceNames: TStrings   имена входов
        TerminalNames: TStrings имена выходов
        JointNames: TStrings    имена узлов

     События:
        OnChange: TNotifyEvent  изменилось состояние входов, могло
                                (но не обязательно!) измениться и состояние
                                выходов
        OnProcessLine: TLineNotifyEvent событие при обработке строки карты с
                                указанным номером
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls;

const
      PIPE_CHANGE = WM_USER + 100;
      MAXSOURCES = 100;
      MAXTERMINALS = 100;
      RJOINT = 5;
      XYCOMB = 10000;

type

   TChainType = (ctUp, ctDown, ctLeft, ctRight, ctJoint, ctHide,
                 ctSource, ctTerminal, ctGo, ctNone);

   PChain = ^TChain;
   TChain = record
     x, y, len: integer;
     cType, cTypePrev: TChainType;
     BeforeTerminal: Boolean;
     stage: integer;
     nSource: integer;
   end;

   EPipesError = class(Exception);
   TLineNotifyEvent = procedure(Sender: TObject; LineNo: integer) of object;

   TkpPipeline = class(TCustomPanel)
   private
     FMapList: TStrings;
     FChains: TList;
     FTimer: TTimer;
     FAnimate: Boolean;
     FFlowColor, FAltColor, FBkColor, FJointColor: TColor;
     FBitmap: TBitmap;
     FBkBitmap: TBitmap;
     FTickLength: integer;
     FPipeWidth: integer;
     FStartStage: integer;
     FMoveStep: integer;
     FSourceState: array[0..MAXSOURCES-1] of Boolean;
     FTerminalLink: array[0..MAXTERMINALS-1] of integer;
     FSources, FTerminals, FJoints: TStrings;
     FOnChange: TNotifyEvent;
     FOnProcessLine: TLineNotifyEvent;
     FNotifyList: TList;
     FClipRgn: HRGN;
     FHasLabels: Boolean;
   protected
     procedure Paint; override;
     procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
     procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
     procedure Loaded; override;
     procedure SetAnimate(NewAnimate: Boolean);
     function  GetColor: TColor;
     procedure SetColor(NewColor: TColor);
     procedure SetAltColor(NewAltColor: TColor);
     procedure SetFlowColor(NewFlowColor: TColor);
     procedure SetBkColor(NewBkColor: TColor);
     procedure SetBkBitmap(NewBkBitmap: TBitmap);
     procedure SetJointColor(NewJointColor: TColor);
     procedure SetTickLength(NewTickLength: integer);
     procedure SetPipeWidth(NewPipeWidth: integer);
     procedure SetMapList(NewMapList: TStrings);
     function  GetPeriod: integer;
     procedure SetPeriod(NewPeriod: integer);
     procedure SetMoveStep(NewMoveStep: integer);
     function  GetSourceState(Index: integer): Boolean;
     procedure SetSourceState(Index: integer; Active: Boolean);
     function  GetTerminalState(Index: integer): Boolean;
     function  GetSourceStateByName(Name: string): Boolean;
     procedure SetSourceStateByName(Name: string; Active: Boolean);
     function  GetTerminalStateByName(Name: string): Boolean;
     procedure SetHasLabels(NewHasLabels: Boolean);
     function  GetMargin: integer;
     procedure OnTimer(Sender: TObject);
     procedure FontChanged(Sender: TObject);
     procedure ClearChainList;
     procedure Parse(s: string; var cType: TChainType; var n1, n2: integer; var Name: string);
     procedure BuildPipeline;
     function  CalcChainRect(var x0, y0: integer; len: integer; cType, cTypePrev: TChainType;
                             IsLast, BeforeTerminal: Boolean): TRect;
     procedure DrawBorder(Cvs: TCanvas; x0, y0, len: integer; cType, cTypePrev: TChainType);
     procedure RefreshPicture;
     procedure Redraw(Cvs: TCanvas; FullRepaint: Boolean);
     procedure RedrawAll; virtual;
   public
     constructor Create(AOwner: TComponent); override;
     destructor  Destroy; override;
     procedure   AddNotify(Obj: TControl);
     procedure   RemoveNotify(Obj: TControl);
     procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
     property    SourceState[Index: integer]: Boolean read GetSourceState write SetSourceState;
     property    TerminalState[Index: integer]: Boolean read GetTerminalState;
     property    SourceStateByName[Name: string]: Boolean read GetSourceStateByName write SetSourceStateByName;
     property    TerminalStateByName[Name: string]: Boolean read GetTerminalStateByName;
     property    SourceNames: TStrings read FSources;
     property    TerminalNames: TStrings read FTerminals;
     property    JointNames: TStrings read FJoints;
   published
     property    Animate: Boolean read FAnimate write SetAnimate;
     property    AltColor: TColor read FAltColor write SetAltColor;
     property    FlowColor: TColor read FFlowColor write SetFlowColor;
     property    BkColor: TColor read FBkColor write SetBkColor;
     property    BkBitmap: TBitmap read FBkBitmap write SetBkBitmap;
     property    Color: TColor read GetColor write SetColor;
     property    JointColor: TColor read FJointColor write SetJointColor;
     property    TickLength: integer read FTickLength write SetTickLength;
     property    PipeWidth: integer read FPipeWidth write SetPipeWidth;
     property    MapList: TStrings read FMapList write SetMapList;
     property    Period: integer read GetPeriod write SetPeriod;
     property    MoveStep: integer read FMoveStep write SetMoveStep;
     property    HasLabels: Boolean read FHasLabels write SetHasLabels;
     property    Margin: integer read GetMargin;
     property    OnChange: TNotifyEvent read FOnChange write FOnChange;
     property    OnProcessLine: TLineNotifyEvent read FOnProcessLine write FOnProcessLine;
     property Align;
     property BevelInner;
     property BevelOuter;
     property BevelWidth;
     property BorderWidth;
     property BorderStyle;
//     property Ctl3D;
     property DragCursor;
     property DragMode;
     property Enabled;
     property FullRepaint;
//    property Caption;
     property Font;
     property MouseCapture;
     property ParentColor;
//    property ParentCtl3D;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property TabOrder;
     property TabStop;
     property Visible;
     property OnClick;
     property OnDblClick;
     property OnDragDrop;
     property OnDragOver;
     property OnEndDrag;
     property OnEnter;
     property OnExit;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
     property OnResize;
     property OnStartDrag;
   end;

procedure Register;

implementation


//-------------- constructor -----------------------
constructor TkpPipeline.Create(AOwner: TComponent);
begin

   FMapList := TStringList.Create;
   FChains := TList.Create;

   FSources := TStringList.Create;
   FTerminals := TStringList.Create;
   FJoints := TStringList.Create;

   FBitmap := TBitmap.Create;
   FBkBitmap := TBitmap.Create;

   FNotifyList := TList.Create;

   FTimer := TTimer.Create(nil);
   FTimer.OnTimer := OnTimer;
   FTimer.Interval := 1000;
   FTimer.Enabled := False;

   FMoveStep := 2;
   FTickLength := 5;
   FPipeWidth := 7;
   FFlowColor := clGreen;
   FAltColor := clWhite;
   FBkColor := clBtnFace;
   FJointColor := clYellow;

   inherited;
   ControlStyle := ControlStyle + [csOpaque];
   Font.OnChange := FontChanged;
   Width := 50;
   Height := 50;
end;

//-------------- destructor -----------------------
destructor TkpPipeline.Destroy;
begin
   if FClipRgn <> 0 then DeleteObject(FClipRgn);
   FMapList.Free;
   ClearChainList;
   FChains.Free;
   FTimer.Free;
   FBitmap.Free;
   FBkBitmap.Free;
   FNotifyList.Free;
   FSources.Free;
   FTerminals.Free;
   FJoints.Free;
   inherited;
end;

//-------------- ClearChainList -----------------------
procedure TkpPipeline.ClearChainList;
var i: integer;
    P: PChain;
begin
   for i:=0 to FChains.Count-1 do begin
      P := PChain(FChains[i]);
      Dispose(P);
   end;
   FChains.Clear;
end;

//-------------- AddNotify ------------------
procedure TkpPipeline.AddNotify(Obj: TControl);
var i: integer;
begin
   i := FNotifyList.IndexOf(Obj);
   if i < 0 then FNotifyList.Add(Pointer(Obj));
end;

//-------------- RemoveNotify ------------------
procedure TkpPipeline.RemoveNotify(Obj: TControl);
var i: integer;
begin
   i := FNotifyList.IndexOf(Obj);
   if i >= 0 then FNotifyList.Delete(i);
end;

{============== Notification =========}
procedure TkpPipeline.Notification(AComponent: TComponent; Operation: TOperation);
var i: integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    i := FNotifyList.IndexOf(AComponent);
    if i >= 0  then FNotifyList.Delete(i);
  end;
end;

//-------------- SetBounds -----------------------
procedure TkpPipeline.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
   if (Width <> AWidth)  or (Height <> AHeight) then begin
     FBitmap.Width := AWidth;
     FBitmap.Height := AHeight;
     RedrawAll;
   end;
   inherited;
end;

//-------------- Redraw all ------------------
procedure TkpPipeline.RedrawAll;
var m, i: integer;
begin
  with FBitmap.Canvas do begin
     if FBkBitmap.Width = 0 then
          Brush.Color := FBkColor
     else Brush.Bitmap := FBkBitmap;
     FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  end;
  BuildPipeline;
  Redraw(FBitmap.Canvas, True);

  m := Margin;
  if Assigned(Parent) then begin
     SelectClipRgn(Canvas.Handle, 0);
     BitBlt(Canvas.Handle, m, m, Width-2*m, Height-2*m, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  end;

  for i:=0 to FNotifyList.Count-1 do
    TControl(FNotifyList[i]).Perform(PIPE_CHANGE, Integer(Self), 0);

  if Assigned(FOnChange) and not (csLoading in ComponentState) then
     FOnChange ( Self );

end;

//-------------- Refresh Picture ------------------
procedure TkpPipeline.RefreshPicture;
var m: integer;
begin
  Redraw(FBitmap.Canvas, False);
  m := Margin;
  SelectClipRgn(Canvas.Handle, FClipRgn);
//  OffsetClipRgn(Canvas.Handle, Left, Top); // если делать на основе TPaintBox !!!
  OffsetClipRgn(Canvas.Handle, m, m);
  BitBlt(Canvas.Handle, m, m, Width-2*m, Height-2*m, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

//-------------- Parse ------------------
procedure TkpPipeline.Parse(s: string; var cType: TChainType; var n1, n2: integer;
                         var Name: string);
  //---------- GetWord ------------------------
  function GetWord(var s: string): string;
  var p: integer;
  begin
    s := Trim(s);
    p := Pos(' ', s);
    if p = 0 then begin Result := s; s := ''; end
    else begin          Result := Copy(s, 1, p-1); s := Copy(s, p+1, 999); end;
  end;
begin
    cType := ctNone; n1 := -1; n2 := -1; Name := '';
    s := Trim(s);
    if Length(s) < 1 then Exit;
    case s[1] of
       's': cType := ctSource;
       't': cType := ctTerminal;
       'j': cType := ctJoint;
       'g': cType := ctGo;
       'h': cType := ctHide;
       'u': cType := ctUp;
       'd': cType := ctDown;
       'l': cType := ctLeft;
       'r': cType := ctRight;
    end;
    Delete(s, 1, 1);
    n1 := StrToIntDef(GetWord(s), -1);
    n2 := StrToIntDef(GetWord(s), -1);
    Name := Trim(s);
end;

//------------- DRAW BORDER ---------------------------------------
procedure TkpPipeline.DrawBorder(Cvs: TCanvas; x0, y0, len: integer; cType, cTypePrev: TChainType);
var w: integer;
begin
     w := FPipeWidth div 2;
     with Cvs do begin
       Pen.Color := clBlack;
       if (cType = cTypePrev)  or (not (cTypePrev in [ctUp,ctDown,ctLeft,ctRight])) then begin
         case cType of
          ctUp:   begin
                 MoveTo(x0-w,y0); LineTo(x0-w,y0-len-1);
                 MoveTo(x0+w,y0); LineTo(x0+w,y0-len-1);
                 end;
          ctDown: begin
                 MoveTo(x0-w,y0); LineTo(x0-w,y0+len+1);
                 MoveTo(x0+w,y0); LineTo(x0+w,y0+len+1);
                 end;
          ctLeft: begin
                 MoveTo(x0,y0-w); LineTo(x0-len-1,y0-w);
                 MoveTo(x0,y0+w); LineTo(x0-len-1,y0+w);
                 end;
          ctRight: begin
                 MoveTo(x0,y0-w); LineTo(x0+len+1,y0-w);
                 MoveTo(x0,y0+w); LineTo(x0+len+1,y0+w);
                 end;
         end
       end;
       if (cType <> cTypePrev) and (cTypePrev <> ctSource) then begin
         case cType of
          ctUp:   begin
                 MoveTo(x0-w,y0+w); LineTo(x0+w+1,y0+w);
                 if cTypePrev = ctRight then begin
                    MoveTo(x0-w,y0-w); LineTo(x0-w,y0-len-1);
                    MoveTo(x0+w,y0+w); LineTo(x0+w,y0-len-1);
                 end else begin
                    MoveTo(x0-w,y0+w); LineTo(x0-w,y0-len-1);
                    MoveTo(x0+w,y0-w); LineTo(x0+w,y0-len-1);
                 end;
                 end;
          ctDown: begin
                 MoveTo(x0-w,y0-w); LineTo(x0+w+1,y0-w);
                 if cTypePrev = ctRight then begin
                    MoveTo(x0-w,y0+w); LineTo(x0-w,y0+len+1);
                    MoveTo(x0+w,y0-w); LineTo(x0+w,y0+len+1);
                 end else begin
                    MoveTo(x0-w,y0-w); LineTo(x0-w,y0+len+1);
                    MoveTo(x0+w,y0+w); LineTo(x0+w,y0+len+1);
                 end;
                 end;
          ctLeft: begin
                 MoveTo(x0+w,y0-w); LineTo(x0+w,y0+w+1);
                 if cTypePrev = ctUp then begin
                    MoveTo(x0+w,y0-w); LineTo(x0-len-1,y0-w);
                    MoveTo(x0-w,y0+w); LineTo(x0-len-1,y0+w);
                 end else begin
                    MoveTo(x0-w,y0-w); LineTo(x0-len-1,y0-w);
                    MoveTo(x0+w,y0+w); LineTo(x0-len-1,y0+w);
                 end;
                 end;
          ctRight: begin
                 MoveTo(x0-w,y0-w); LineTo(x0-w,y0+w+1);
                 if cTypePrev = ctUp then begin
                    MoveTo(x0-w,y0-w); LineTo(x0+len+1,y0-w);
                    MoveTo(x0+w,y0+w); LineTo(x0+len+1,y0+w);
                 end else begin
                    MoveTo(x0+w,y0-w); LineTo(x0+len+1,y0-w);
                    MoveTo(x0-w,y0+w); LineTo(x0+len+1,y0+w);
                 end;
                 end;
         end;
       end;
     end;
end;

//-------------- CalcChainRect ------------------
function TkpPipeline.CalcChainRect(
             var x0, y0: integer; len: integer; cType, cTypePrev: TChainType;
             IsLast, BeforeTerminal: Boolean): TRect;
var w, x1, y1, x2, y2, xi, yi, temp: integer;
begin
    w := FPipeWidth div 2 - 1;
    xi := x0; yi := y0;
    x1 := x0; x2 := x0; y1 := y0; y2 := y0;
    case cType of
       ctUp:    begin
               yi := y0 - len;
               x1 := x0 - w; x2 := x0 + w + 1; y1 := y0; y2 := yi;
               if IsLast and (not BeforeTerminal) then y2 := y2 - w;
               end;
       ctDown:  begin
               yi := y0 + len;
               x1 := x0 - w; x2 := x0 + w + 1; y1 := y0; y2 := yi;
               if IsLast and (not BeforeTerminal) then y2 := y2 + w + 1;
               end;
       ctLeft:  begin
               xi := x0 - len;
               x1 := x0 + 1; x2 := xi; y1 := y0 - w; y2 := y0 + w + 1;
               if IsLast and (not BeforeTerminal) then x2 := x2 - w;
               end;
       ctRight: begin
               xi := x0 + len;
               x1 := x0; x2 := xi + 1; y1 := y0 - w; y2 := y0 + w + 1;
               if IsLast and (not BeforeTerminal) then x2 := x2 + w;
               end;
    end;
    if x1 > x2 then begin temp := x1; x1 := x2; x2 := temp; end;
    if y1 > y2 then begin temp := y1; y1 := y2; y2 := temp; end;
    Result := Rect(x1, y1, x2, y2);
    x0 := xi; y0 := yi;
end;

//-------------- Redraw ------------------
var hideR: array[1..8] of integer = (1, 2, 3, 4, 5, 7, 9, 12);
procedure TkpPipeline.Redraw(Cvs: TCanvas; FullRepaint: Boolean);
var i, xy, x, y, stage: integer;
    P, PPrev: PChain;
    Skip: Boolean;
    rgn: HRGN;
  //------------- DRAW SEGMENT ---------------------------------------
  procedure DrawSegment(var x0, y0, len: integer; cType, cTypePrev: TChainType;
                        Active, IsLast, BeforeTerminal: Boolean);
  var rct: TRect;
  begin
    rct := CalcChainRect(x0, y0, len, cType, cTypePrev, IsLast, BeforeTerminal);
    with Cvs do begin
       if Active then
            Brush.Color := FFlowColor
       else Brush.Color := FAltColor;
       Pen.Color := Brush.Color;
       Rectangle(rct.Left, rct.Top, rct.Right, rct.Bottom);
       if FullRepaint then begin
          rgn := CreateRectRgn(rct.Left, rct.Top, rct.Right, rct.Bottom);
          CombineRgn(FClipRgn, FClipRgn, rgn, RGN_OR);
          DeleteObject(rgn);
       end;
    end;
  end;
  //------------- DRAW PIPE ---------------------------------------
  procedure DrawPipe(x, y, len: integer; cType, cTypePrev: TChainType;
                     SourceState, BeforeTerminal: Boolean);
  var lenSeg: integer;
      IsTick: Boolean;
  begin
     IsTick := stage > FTickLength;
     DrawBorder ( Cvs, x, y, len, cType, cTypePrev );
     while len > 0 do begin
       if stage > FTickLength then
            lenSeg := stage - FTickLength
       else lenSeg := stage;
       if lenSeg > len then lenSeg := len;
       stage := stage - lenSeg;
       if stage = 0 then stage := 2*FTickLength;
       DrawSegment(x, y, lenSeg, cType, cTypePrev,
                      IsTick and SourceState, len = lenSeg, BeforeTerminal);
       if stage mod FTickLength = 0 then IsTick := not IsTick;
       Dec(len, lenSeg);
       cTypePrev := cType;
     end;
  end;
  //------------------- DRAW NAME -------------------------
  procedure DrawName(x, y: integer; Name: string; Dir: TChainType);
  var w, m: integer;
      rct: TRect;
  begin
     with Cvs do begin
       w := TextWidth(Name) div 2;
       Pen.Color := clBlack;
       Font.Assign(Self.Font);
       Brush.Color := FJointColor;
       case Dir of
         ctUp: x := x - w - 2;
         ctRight: x := x + FPipeWidth;
       end;
       y := y - FPipeWidth div 2 - 3 - TextHeight(Name);
       rct := Rect(x, y, x+w, y+5);
       m := Margin;
       if x < 2 then OffSetRect(rct, 2-x, 0);
       DrawText(Cvs.Handle, PChar(Name), Length(Name), rct,
                DT_SINGLELINE or DT_CALCRECT);
       if rct.Right+2+2*m > Width then OffSetRect(rct, Width-rct.Right-2-2*m, 0);
       Rectangle ( rct.Left-2, rct.Top-2, rct.Right+2, rct.Bottom+2);
       DrawText(Cvs.Handle, PChar(Name), Length(Name), rct,
                DT_SINGLELINE);
     end;
  end;
  //---------------------------------------------------------
  procedure DrawHideArea ( x, y: integer );
  var k, r: integer;
  begin
     with Cvs do begin
        Pen.Color := FBkColor;
        Brush.Style := bsClear;
        for k:=1 to 8 do begin
          r := hideR[k];
          Ellipse(x-r, y-r, x+r+1, y+r+1);
        end;
        if FullRepaint then begin
          r := hideR[8];
          rgn := CreateEllipticRgn(x-r, y-r, x+r+1, y+r+1);
          CombineRgn(FClipRgn, FClipRgn, rgn, RGN_OR);
          DeleteObject(rgn);
        end;
     end;
  end;
  //---------------------------------------------------------
  procedure DrawJoint ( x, y: integer; Active: Boolean );
  begin
    with Cvs do begin
       Pen.Color := FJointColor;
       if Active then
            Brush.Color := FFlowColor
       else Brush.Color := FAltColor;
       Ellipse(x-RJOINT, y-RJOINT, x+RJOINT, y+RJOINT);
       Pen.Color := clBlack;
       Brush.Style := bsClear;
       Ellipse(x-RJOINT-1, y-RJOINT-1, x+RJOINT+1, y+RJOINT+1);
       Brush.Style := bsSolid;
       if FullRepaint then begin
          rgn := CreateEllipticRgn(x-RJOINT-1, y-RJOINT-1, x+RJOINT+1, y+RJOINT+1);
          CombineRgn(FClipRgn, FClipRgn, rgn, RGN_OR);
          DeleteObject(rgn);
       end;
    end;
  end;
  //---------------------------------------------------------
begin

   FTimer.Enabled := False;
   Skip := False;

   if FullRepaint then begin
      if FClipRgn <> 0 then DeleteObject(FClipRgn);
      FClipRgn := CreateRectRgn(0, 0, 0, 0);
   end;

   PPrev := nil;
   for i:=0 to FChains.Count-1 do begin
      P := PChain(FChains[i]);
      case P^.cType of
        ctHide:
           Skip := True;
        ctLeft, ctRight, ctUp, ctDown:
           begin
           stage := P^.stage + FStartStage - 2*FTickLength;
           if stage <= 0 then stage := stage + 2*FTickLength;
           DrawPipe(P^.x, P^.y, P^.len, P^.cType, P^.cTypePrev,
                     SourceState[P^.nSource], P^.BeforeTerminal);
           if Skip then DrawHideArea ( PPrev^.x, PPrev^.y );
           Skip := False;
        end;
      end;
      PPrev := P;
   end;

   for i:=0 to FChains.Count-1 do begin
      P := PChain(FChains[i]);
      if P^.cType = ctJoint then
         DrawJoint(P^.x, P^.y, SourceState[P^.nSource]);
   end;

   if FullRepaint and FHasLabels then begin
     for i:=0 to FSources.Count-1 do begin
       if FSources[i] = '' then continue;
       xy := integer(FSources.Objects[i]);
       x := xy div XYCOMB;
       y := xy mod XYCOMB;
       DrawName ( x, y, FSources[i], ctUp );
     end;
     for i:=0 to FTerminals.Count-1 do begin
       if FTerminals[i] = '' then continue;
       xy := integer(FTerminals.Objects[i]);
       x := xy div XYCOMB;
       y := xy mod XYCOMB;
       DrawName ( x, y, FTerminals[i], ctUp );
     end;
     for i:=0 to FJoints.Count-1 do begin
       if FJoints[i] = '' then continue;
       P := PChain(FJoints.Objects[i]);
       DrawName ( P^.x, P^.y, FJoints[i], ctRight );
     end;
   end;

   FTimer.Enabled := FAnimate;
end;

//-------------- BuildPipeline ------------------
procedure TkpPipeline.BuildPipeline;
var x, y, i, n1, n2,
    k, stage, nSource, CurSource, nTerminal: integer;
    BeforeTerminal: Boolean;
    CurType, PrevType: TChainType;
    P: PChain;
    s, Name: string;
    //---------------- CreateChain ----------------------
    function CreateChain ( len: integer ): PChain;
    var P: PChain;
    begin
       New(P);
       P^.x := x; P^.y := y;
       P^.len := len;
       P^.cType := CurType;
       P^.cTypePrev := PrevType;
       P^.stage := stage;
       P^.nSource := CurSource;
       P^.BeforeTerminal := BeforeTerminal;
       Result := P;
    end;
    //---------------------------------------------------
begin

  FTimer.Enabled := False;

  ClearChainList;
  FSources.Clear;
  FTerminals.Clear;
  FJoints.Clear;

  PrevType := ctNone;
  nSource := -1;
  CurSource := -1;
  nTerminal := -1;

  FStartStage := 2*FTickLength;

  for i:=0 to FMapList.Count-1 do begin

    if Assigned(FOnProcessLine) then FOnProcessLine(Self, i);
    Parse(FMapList[i], CurType, n1, n2, Name);

    BeforeTerminal := False;
    if (i < FMapList.Count-1) then begin
       s := Trim(FMapList[i+1]);
       BeforeTerminal := (Length(s) > 0)  and  (s[1] = 't');
    end;

    case CurType of
      ctJoint:  begin
               Name := Trim(Copy(FMapList[i],2,999));
               if (Name <> '') and (FJoints.IndexOf(Name) >= 0) then
                  raise EPipesError.CreateFmt('Узел ''%s'' уже есть в схеме', [Name]);
               P := CreateChain(0);
               FChains.Add(Pointer(P));
               FJoints.AddObject(Name, TObject(P));
               end;
      ctHide:   begin
               P := CreateChain(0);
               FChains.Add(Pointer(P));
               end;
      ctGo:     begin
               if n1 >= 0 then begin
                  x := n1; y := n2;
               end else begin
                  Name := Trim(Copy(FMapList[i],2,999));
                  k := FJoints.IndexOf(Name);
                  if k < 0 then
                     raise EPipesError.CreateFmt('Узел ''%s'' не найден в схеме', [Name]);
                  P := PChain(FJoints.Objects[k]);
                  x := P^.x; y := P^.y;
                  stage := P^.stage;
                  CurSource := P^.nSource;
               end;
               end;
      ctSource: begin
               if (Name <> '') and (FSources.IndexOf(Name) >= 0) then
                  raise EPipesError.CreateFmt('Источник ''%s'' уже есть в схеме', [Name]);
               x := n1; y := n2;
               Inc(nSource);
               FSources.AddObject(Name, TObject(x*XYCOMB+y));
               CurSource := nSource;
               stage := FStartStage;
               end;
      ctTerminal: begin
               Name := Trim(Copy(FMapList[i],2,999));
               if (Name <> '') and (FTerminals.IndexOf(Name) >= 0) then
                  raise EPipesError.CreateFmt('Терминал ''%s'' уже есть в схеме', [Name]);
               Inc(nTerminal);
               FTerminalLink[nTerminal] := CurSource;
               FTerminals.AddObject(Name, TObject(x*XYCOMB+y));
               end;
      ctNone:   begin x := n1; y := n2; end;
      else     begin
               P := CreateChain( n1 );
               FChains.Add(Pointer(P));
               CalcChainRect(x, y, n1, CurType, PrevType, False, BeforeTerminal);
               end;
    end;

    PrevType := CurType;
  end;

  FTimer.Enabled := FAnimate;

end;

//-------------- WMEraseBkgnd ------------------
procedure TkpPipeline.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
   Message.Result := 1; 
end;

//-------------- GetMargin ------------------
function TkpPipeLine.GetMargin: integer;
begin
   Result := integer(BevelOuter <> bvNone)*BevelWidth +
             integer(BevelInner <> bvNone)*BevelWidth + BorderWidth;
end;

//-------------- Paint ------------------
procedure TkpPipeline.Paint;
var Rct: TRect;
    TopColor, BottomColor: TColor;
    m: integer;
  //-------------------------------------------------
  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;
  //-------------------------------------------------
begin
  m := Margin;
  SelectClipRgn(Canvas.Handle, 0);
  Canvas.Draw(m, m, FBitmap);
  Rct := GetClientRect;
  if BevelOuter <> bvNone then begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rct, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rct, Color, Color, BorderWidth);
  if BevelInner <> bvNone then begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rct, TopColor, BottomColor, BevelWidth);
  end;
end;

//-------------- SetAnimate ------------------
procedure TkpPipeline.SetAnimate(NewAnimate: Boolean);
begin
   if FAnimate = NewAnimate then Exit;
   FAnimate := NewAnimate;
   FTimer.Enabled := NewAnimate;
end;

//-------------- OnTimer ------------------
procedure TkpPipeline.OnTimer(Sender: TObject);
begin
   FStartStage := (FStartStage + FMoveStep) mod (2*FTickLength);
   RefreshPicture;
end;

//-------------- SetAltColor ------------------
procedure TkpPipeline.SetAltColor(NewAltColor: TColor);
begin
   if NewAltColor = FAltColor then Exit;
   FAltColor := NewAltColor;
   RefreshPicture;
end;

//-------------- SetFlowColor ------------------
procedure TkpPipeline.SetFlowColor(NewFlowColor: TColor);
begin
   if NewFlowColor = FFlowColor then Exit;
   FFlowColor := NewFlowColor;
   RefreshPicture;
end;

//-------------- SetBkColor ------------------
procedure TkpPipeline.SetBkColor(NewBkColor: TColor);
begin
   if NewBkColor = FBkColor then Exit;
   FBkColor := NewBkColor;
   RedrawAll;
end;

//-------------- GetColor ------------------
function  TkpPipeline.GetColor: TColor;
begin
   Result := inherited Color;
end;

//-------------- SetColor ------------------
procedure TkpPipeline.SetColor(NewColor: TColor);
begin
   if inherited Color = NewColor then Exit;
   inherited Color := NewColor;
   Invalidate;
end;

//-------------- SetBkBitmap ------------------
procedure TkpPipeline.SetBkBitmap(NewBkBitmap: TBitmap);
begin
   FBkBitmap.Assign(NewBkBitmap);
   if (FBkBitmap.Width > 0) and (FBkBitmap.Height > 0) then
      FBkColor := FBkBitmap.Canvas.Pixels[0,0];
   RedrawAll;
end;

//-------------- SetJointColor ------------------
procedure TkpPipeline.SetJointColor(NewJointColor: TColor);
begin
   if NewJointColor = FJointColor then Exit;
   FJointColor := NewJointColor;
   RefreshPicture;
end;

//-------------- SetTickLength ------------------
procedure TkpPipeline.SetTickLength(NewTickLength: integer);
begin
   if NewTickLength = FTickLength then Exit;
   FTickLength := NewTickLength;
   FStartStage := 2*FTickLength;
   RefreshPicture;
end;

//-------------- SetPipeWidth ------------------
procedure TkpPipeline.SetPipeWidth(NewPipeWidth: integer);
begin
   if NewPipeWidth = FPipeWidth then Exit;
   FPipeWidth := NewPipeWidth;
   RedrawAll;
end;

//-------------- Loaded ------------
procedure TkpPipeline.Loaded;
begin
   RedrawAll;
end;

//-------------- GetMapList ------------------
procedure TkpPipeline.SetMapList(NewMapList: TStrings);
var i: integer;
begin
   FMapList.Assign(NewMapList);
   for i:=0 to MAXSOURCES-1 do FSourceState[i] := False;
   RedrawAll;
end;

//-------------- GetSourceState ------------------
function TkpPipeline.GetSourceState(Index: integer): Boolean;
begin
   if (Index < 0)  or (Index >= MAXSOURCES) then
      raise EPipesError.CreateFmt('Неизвестный вход #%d', [Index]);
   Result := FSourceState[Index];
end;

//-------------- SetSourceState ------------------
procedure TkpPipeline.SetSourceState(Index: integer; Active: Boolean);
var i: integer;
begin
   if (Index < 0)  or (Index >= MAXSOURCES) then
      raise EPipesError.CreateFmt('Неизвестный вход #%d', [Index]);
  if FSourceState[Index] = Active then Exit;
  FSourceState[Index] := Active;
  RefreshPicture;
  for i:=0 to FNotifyList.Count-1 do
     TControl(FNotifyList[i]).Perform(PIPE_CHANGE, Integer(Self), Index);
  if Assigned(FOnChange) then FOnChange ( Self );
end;

//-------------- GetTerminalState ------------------
function TkpPipeline.GetTerminalState(Index: integer): Boolean;
var nSource: integer;
begin
   Result := False;
   if (Index < 0)  or (Index >= MAXTERMINALS) then
      raise EPipesError.CreateFmt('Неизвестный выход #%d', [Index]);
   nSource := FTerminalLink[Index];
   if (nSource < 0)  or (nSource >= MAXSOURCES) then Exit;
   Result := FSourceState[nSource];
end;

//-------------- States by Name ------------------
function  TkpPipeline.GetSourceStateByName(Name: string): Boolean;
var n: integer;
begin
   n := FSources.IndexOf(Name);
   if n < 0 then
      raise EPipesError.CreateFmt('Неизвестный вход ''%s''', [Name]);
   Result := GetSourceState(n);
end;
procedure TkpPipeline.SetSourceStateByName(Name: string; Active: Boolean);
var n: integer;
begin
   n := FSources.IndexOf(Name);
   if n < 0 then
      raise EPipesError.CreateFmt('Неизвестный вход ''%s''', [Name]);
   SetSourceState(n, Active);
end;
function  TkpPipeline.GetTerminalStateByName(Name: string): Boolean;
var n: integer;
begin
   n := FTerminals.IndexOf(Name);
   if n < 0 then
      raise EPipesError.CreateFmt('Неизвестный выход ''%s''', [Name]);
   Result := GetTerminalState(n);
end;

//-------------- GetPeriod ------------------
function TkpPipeline.GetPeriod: integer;
begin
   Result := FTimer.Interval;
end;

//-------------- SetPeriod ------------------
procedure TkpPipeline.SetPeriod(NewPeriod: integer);
begin
   if NewPeriod = FTimer.Interval then Exit;
   FTimer.Interval := NewPeriod;
end;

//-------------- SetMoveStep ------------------
procedure TkpPipeline.SetMoveStep(NewMoveStep: integer);
begin
   if NewMoveStep = FMoveStep then Exit;
   FMoveStep := NewMoveStep;
end;

//-------------- SetHasLabels ---------------------
procedure TkpPipeline.SetHasLabels(NewHasLabels: Boolean);
begin
   if FHasLabels = NewHasLabels then Exit;
   FHasLabels := NewHasLabels;
   RedrawAll;
end;

//-------------- FontChanged ------------------
procedure TkpPipeline.FontChanged(Sender: TObject);
begin
  if HasLabels then RedrawAll;
end;

{---------------------------------------------------------
    REGISTER
----------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('KP', [TkpPipeline]);
end;


end.
