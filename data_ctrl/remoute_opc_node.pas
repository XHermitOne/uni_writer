{
Модуль удаленного узла OPC сервера

Версия: 0.0.3.1
}

unit remoute_opc_node;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ActiveX,
    obj_proto, dictionary, strfunc,
    opc_client, tag_list;

const
  RESERV_PROPERTIES: Array [1..6] Of String = ('type', 'name', 'description', 'opc_server', 'opc_host', 'topic');

type
  {
  Класс взаимодействия с удаленным OPC сервером.
  }
  TICRemouteOPCNode = class(TICObjectProto)

  private
    { Объект OPC клиента }
    FOPCClient: TOPCClient;

  public
    constructor Create;
    destructor Destroy; override;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

    //{ Фунция чтения данных }
    //function Read(aValues: TStringList): TStringList; override;
    //{ Фунция записи данных }
    ////function Write(aValues: TStringList): Boolean; override;
    //function Write(aAddresses: TStringList; aValues: TStringList): Boolean; override;

    {
    Фунция чтения данных
    @param sAddresses Список адресов для чтения
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function Read(sAddresses: TStringList; dtTime: TDateTime = 0): TStringList; override;
    //{
    //Чтение значений по адресам
    //@param sAddresses Массив адресов для чтения
    //@param dtTime: Время актуальности за которое необходимо получить данные.
    //              Если не определено, то берется текущее системное время.
    //@return Список прочитанных значений.
    //}
    //function ReadAddresses(sAddresses: Array Of String; dtTime: TDateTime = 0): TStringList; override;
    //{
    //Чтение значения по адресу
    //@param sAddress Строка адреса для чтения
    //@param dtTime: Время актуальности за которое необходимо получить данные.
    //              Если не определено, то берется текущее системное время.
    //@return Прочитанное значение в виде строки.
    //}
    //function ReadAddress(sAddress: AnsiString; dtTime: TDateTime = 0): AnsiString; virtual;
    //{
    //Чтение всех внутренних данных, описанных в свойствах.
    //@param dtTime: Время актуальности за которое необходимо получить данные.
    //              Если не определено, то берется текущее системное время.
    //@return Список прочитанных значений.
    //}
    //function ReadAll(dtTime: TDateTime = 0): TStringList; virtual;

    //{
    //Чтение значений исторических данных по адресам
    //@param sAddresses Массив адресов для чтения
    //@param dtTime: Время актуальности за которое необходимо получить данные.
    //               Если не определено, то берется текущее системное время.
    //@param iValueTimeCount: Количество считываемых записей.
    //@param sValueTimeTick: Период регистрации контроллера в формате yyyy-mm-dd hh:nn:ss в виде строки.
    //@return Список прочитанных значений.
    //}
    //function ReadHistoryAddresses(sAddresses: Array Of String; dtTime: TDateTime = 0; iValueTimeCount: Integer = 0; sValueTimeTick: AnsiString = ''): TStringList; virtual;
    //
    {
    Фунция записи данных
    @param sAddresses Список адресов для записи
    @param aValues Список значений для записи
    @param dtTime: Время актуальности данных.
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function Write(sAddresses, aValues: TStringList; dtTime: TDateTime = 0): Boolean; override;
    //{
    //Запись значений по адресам
    //@param sAddresses Массив адресов для записи
    //@param aValues Массив значений для записи
    //@param dtTime: Время актуальности данных.
    //              Если не определено, то берется текущее системное время.
    //@return Результат записи - True - запись прошла успешно False - ошибка
    //}
    //function WriteAddresses(sAddresses,aValues: Array Of String; dtTime: TDateTime = 0): Boolean; virtual;
    //{
    //Запись значения по адресу
    //@param sAddress Значение адреса для записи
    //@param aValue Значение для записи в строковом представлении
    //@param dtTime: Время актуальности данных.
    //              Если не определено, то берется текущее системное время.
    //@return Результат записи - True - запись прошла успешно False - ошибка
    //}
    //function WriteAddress(sAddress, aValue: AnsiString; dtTime: TDateTime = 0): Boolean; virtual;
    //{
    //Запись всех внутренних данных
    //@param dtTime: Время актуальности данных.
    //              Если не определено, то берется текущее системное время.
    //@return Результат записи - True - запись прошла успешно False - ошибка
    //}
    //function WriteAll(dtTime: TDateTime = 0): Boolean; virtual;

end;

implementation

uses
    LCLIntf, // Для вычисления времени выполнения
    log;

constructor TICRemouteOPCNode.Create;
begin
     inherited Create;
     FOPCClient := nil;
end;

destructor TICRemouteOPCNode.Destroy;
begin
  if FOPCClient <> nil then
    FOPCClient.Destroy;
  inherited Destroy;
end;


{
Фунция чтения данных
}
function TICRemouteOPCNode.Read(sAddresses: TStringList; dtTime: TDateTime = 0): TStringList;
//function TICRemouteOPCNode.Read(aValues: TStringList): TStringList;
begin
  result := nil;
end;

{
Фунция записи данных
}
function TICRemouteOPCNode.Write(sAddresses, aValues: TStringList; dtTime: TDateTime = 0): Boolean;
//function TICRemouteOPCNode.Write(aAddresses: TStringList; aValues: TStringList): Boolean;
begin
  result := False;
end;

{ Выбрать описания тегов из свойств }
function TICRemouteOPCNode.CreateTags(): TStrDictionary;
var
  i: Integer;
  key, value: AnsiString;
  tags: TStrDictionary;
begin
  tags := TStrDictionary.Create;
  for i := 0 to Properties.Count - 1 do
  begin
    key := Properties.GetKey(i);
    if not IsStrInList(key, RESERV_PROPERTIES) then
    begin
      value := Properties.GetStrValue(key);
      tags.AddStrValue(key, value);
    end;
  end;
  result := tags;
end;

end.

