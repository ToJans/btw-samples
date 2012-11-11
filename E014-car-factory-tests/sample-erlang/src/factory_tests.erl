-module(factory_tests).
-include_lib("eunit/include/eunit.hrl").

-include("contracts.hrl").
-import(contracts,[format/1]).
-import(factory,[do/2,handle/2]).

-define(ID,"factory/11").

-define(GIVEN(Events),State=lists:foldl(fun(Evt,State) -> handle(State,Evt) end,undefined,Events)).
-define(WHEN(Command),Out=do(State,Command)).
-define(EXPECT(Event),?assertEqual(Out,Event)).

open_factory_test_() -> [
{"Open a new factory",fun() ->
  ?GIVEN([]),
  ?WHEN(#openFactory{id=?ID}),
  ?EXPECT(#factoryOpened{id=?ID})
end},
{"Open a factory that has been opened before",fun() ->
  ?GIVEN([#factoryOpened{id=?ID}]),
  ?WHEN(#openFactory{id=?ID}),
  ?EXPECT(factory_already_created)
end}
].

assign_employee_to_factory_test_() -> [
{"Empty factory allows any employee not Bender to be assigned", fun()->
    ?GIVEN([#factoryOpened{id=?ID}]),
    ?WHEN(#assignEmployeeToFactory{id=?ID,employeeName="fry"}),
    ?EXPECT(#employeeAssignedToFactory{id=?ID,employeeName="fry"})
 end},
{"Duplicate employee name is assigned but not allowed",fun()->
    ?GIVEN([#factoryOpened{id=?ID},
            #employeeAssignedToFactory{id=?ID,employeeName="fry"}]),
    ?WHEN(#assignEmployeeToFactory{id=?ID,employeeName="fry"}),
    ?EXPECT(employee_name_already_taken)
 end},
{"No employee named 'Bender' is allowed to be assigned",fun()->
    ?GIVEN([#factoryOpened{id=?ID}]),
    ?WHEN(#assignEmployeeToFactory{id=?ID,employeeName="Bender"}),
    ?EXPECT(bender_employee)
 end},
{"Can't assign an employee to an unopened factory",fun()->
    ?GIVEN([]),
    ?WHEN(#assignEmployeeToFactory{id=?ID,employeeName="fry"}),
    ?EXPECT(factory_is_not_open)
 end}
].

receive_shipment_in_cargo_bay_test_() -> [
 {"A correct shipment received anouncement is made",fun()->
   ?GIVEN([#factoryOpened{id=?ID},
           #employeeAssignedToFactory{id=?ID,employeeName="yoda"}]),
   ?WHEN  (#receiveShipmentInCargoBay{id=?ID,shipmentName="shipment-777",carParts=[{engine,3}]}),
   ?EXPECT(#shipmentReceivedInCargoBay{id=?ID,shipment={"shipment-777",[{engine,3}]}})
 end},
 {"An empty shipment is not allowed",fun()->
   ?GIVEN([#factoryOpened{id=?ID},
           #employeeAssignedToFactory{id=?ID,employeeName="yoda"}]),
   ?WHEN  (#receiveShipmentInCargoBay{id=?ID,shipmentName="shipment-777",carParts=[]}),
   ?EXPECT(empty_inventory_shipment)
 end},
 {"A shipment can not be received when there are no employees assigned to a factory",fun()->
   ?GIVEN([#factoryOpened{id=?ID}]),
   ?WHEN  (#receiveShipmentInCargoBay{id=?ID,shipmentName="shipment-777",carParts=[{engine,3}]}),
   ?EXPECT(factory_is_unmanned)
 end},
 {"A shipment can not be received when the cargo bay is full",fun()->
   ?GIVEN([#factoryOpened{id=?ID},
           #employeeAssignedToFactory{id=?ID,employeeName="fry"},
           #shipmentReceivedInCargoBay{id=?ID,shipment={"shipment-777",[{engine,3}]}},
           #shipmentReceivedInCargoBay{id=?ID,shipment={"shipment-888",[{wheel,4}]}}]),
   ?WHEN  (#receiveShipmentInCargoBay{id=?ID,shipmentName="shipment-999",carParts=[{engine,2}]}),
   ?EXPECT(cargo_bay_full)
 end},
 {"A shipment can not be received by a factory that is not opened",fun()->
   ?GIVEN([]),
   ?WHEN  (#receiveShipmentInCargoBay{id=?ID,shipmentName="shipment-777",carParts=[{engine,3}]}),
   ?EXPECT(factory_is_not_open)
 end}
].

unpack_and_inventory_a_shipment_test_() ->[
 {"A single shipment is unpacked and an inventory is made",fun()->
   Shipment={"shipment-777",[{engine,3}]},
   ?GIVEN([#factoryOpened{id=?ID},
           #employeeAssignedToFactory{id=?ID,employeeName="fry"},
           #shipmentReceivedInCargoBay{id=?ID,shipment=Shipment}]),
   ?WHEN  (#unpackAndInventoryShipmentInCargoBay{id=?ID,employeeName="fry"}),
   ?EXPECT(#shipmentUnpackedInCargoBay{id=?ID,employeeName="fry",inventoryShipments=[Shipment]})
end },
 {"multiple shipments are unpacked and an inventory is made",fun()->
   Shipment1={"shipment-777",[{engine,3}]},
   Shipment2={"shipment-888",[{wheels,4}]},
   ?GIVEN([#factoryOpened{id=?ID},
           #employeeAssignedToFactory{id=?ID,employeeName="fry"},
           #shipmentReceivedInCargoBay{id=?ID,shipment=Shipment1},
           #shipmentReceivedInCargoBay{id=?ID,shipment=Shipment2}]),
   ?WHEN  (#unpackAndInventoryShipmentInCargoBay{id=?ID,employeeName="fry"}),
   ?EXPECT(#shipmentUnpackedInCargoBay{id=?ID,employeeName="fry",inventoryShipments=[Shipment1,Shipment2]})
end }
].

produce_a_car_test_()->[
 {"Assigning an employee that is not in the factory is an error",fun()-> 
    ?GIVEN([#factoryOpened{id=?ID}]),
    ?WHEN(#produceACar{id=?ID,employeeName="fry",carModel="Ford"}),
    ?EXPECT(unknown_employee)
  end}
].
