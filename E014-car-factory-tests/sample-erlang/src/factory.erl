-module(factory).
-record(factory,{id,employees,blueprints,partsInStock,shipmentsInCargoBay}).
-include("contracts.hrl").
-export([do/2,handle/2]).

%% Open factory
do(undefined,#openFactory{id=Id}) ->
  #factoryOpened{id=Id};

do(undefined,_) ->
  factory_is_not_open;

do(#factory{id=_Id},#openFactory{id=_Id}) ->
  factory_already_created;


%% Assign employee
do(#factory{id=Id},#assignEmployeeToFactory{id=Id,employeeName="Bender"}) ->
  bender_employee;

do(#factory{id=Id,employees=Employees},#assignEmployeeToFactory{id=Id,employeeName=Name}) ->
  case lists:member(Name,Employees) of
    true  -> employee_name_already_taken;
    false -> #employeeAssignedToFactory{id=Id,employeeName=Name}
  end;


%% Receive shipment
do(#factory{id=_Id},#receiveShipmentInCargoBay{id=_Id,carParts=[]}) ->
  empty_inventory_shipment;

do(#factory{id=Id,employees=[]},#receiveShipmentInCargoBay{id=Id}) ->
  factory_is_unmanned;

do(Fact=#factory{id=Id},#receiveShipmentInCargoBay{id=Id,shipmentName=ShipmentName,carParts=CarParts}) ->
  case isCargoBayFull(Fact) of
   true  -> cargo_bay_full;
   false -> #shipmentReceivedInCargoBay{id=Id,shipment={ShipmentName,CarParts}}
  end;

%% unpack and make an inventory of all shipments in the cargo bay
do(Fact=#factory{id=Id,shipmentsInCargoBay=Shipments},#unpackAndInventoryShipmentInCargoBay{id=Id,employeeName=EmployeeName}) ->
case hasEmployee(Fact,EmployeeName) of
  false -> unknown_employee;
  true -> #shipmentUnpackedInCargoBay{id=Id,employeeName=EmployeeName,inventoryShipments=Shipments}
end;


%% Produce a car
do(Fact=#factory{id=Id},#produceACar{id=Id,employeeName=EmployeeName,carModel=CarModel}) ->
  Blueprint = getBlueprint(Fact,carModel),
  ArePartsAvailable=arePartsAvailable(Fact#factory.partsInStock,Blueprint),
  case {hasEmployee(Fact,EmployeeName),Blueprint,ArePartsAvailable} of
    {false,_,_} -> unknown_employee;
    {_,false,_} -> car_model_not_found;
    {_,_,false} -> car_parts_not_in_stock;
    {_,_,true } -> #carProduced{id=Id,employeeName=EmployeeName}
  end.

handle(undefined, #factoryOpened{id=Id}) ->
  #factory{id=Id,employees=[],blueprints=[],shipmentsInCargoBay=[]};

handle(Fact=#factory{id=Id,employees=Employees},#employeeAssignedToFactory{id=Id,employeeName=Name}) ->
  Fact#factory{employees = [Name|Employees]};

handle(Fact=#factory{id=_Id,shipmentsInCargoBay=Shipments},#shipmentReceivedInCargoBay{id=_Id,shipment=Shipment}) ->
  Fact#factory{shipmentsInCargoBay=Shipments++[Shipment]}.

arePartsAvailable(_Factory,_Blueprint = false) ->
  undefined;
arePartsAvailable(_Factory,Blueprint) ->
   {_,RequiredParts} = Blueprint,
   true. 

isCargoBayFull(#factory{shipmentsInCargoBay=Shipments}) ->
  length(Shipments)>=2.

hasEmployee(#factory{employees=Employees},EmployeeName) ->
  lists:member(EmployeeName,Employees).

getBlueprint(#factory{blueprints=Blueprints},CarModel) ->
  lists:keyfind(CarModel,1,Blueprints).
