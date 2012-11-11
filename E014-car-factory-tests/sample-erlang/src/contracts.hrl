-record(openFactory,{id}).
-record(factoryOpened,{id}).
-record(assignEmployeeToFactory,{id,employeeName,carModel}).
-record(employeeAssignedToFactory,{id,employeeName,carModel}).
-record(receiveShipmentInCargoBay,{id,shipmentName, carParts = []}).
-record(shipmentReceivedInCargoBay,{id,shipment}).
-record(curseWordUttered,{theWord,meaning}).
-record(unpackAndInventoryShipmentInCargoBay,{id,employeeName}).
-record(shipmentUnpackedInCargoBay,{id,employeeName, inventoryShipments=[]}).
-record(produceACar,{id,employeeName, carModel}).
-record(carProduced,{id,employeeName, carModel, parts=[]}).
