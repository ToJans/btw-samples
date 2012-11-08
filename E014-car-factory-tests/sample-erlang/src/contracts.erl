-module(contracts).
-include("contracts.hrl").

-export([format/1]).
-define(FMT(RecordType,FormatString), 
	format(Rec=#RecordType{}) -> 
	  FieldNames=record_info(fields, RecordType),
      Values=tl(tuple_to_list(Rec)),
	  list:replace
      PropList=lists:zip(FieldNames,[stringify(X)||X<-Values]),
      subst(FormatString,PropList)
).

?FMT(openFactory,"Open Factory(ID='{id}')");
?FMT(factoryOpened,"Opened Factory(ID='{id}')");
?FMT(assignEmployeeToFactory,"Assign employee '{employeeName}'");
?FMT(employeeAssignedToFactory,"new worker joins our forces: '{employeeName}'");
?FMT(curseWordUttered,"'{theWord}' was heard within the walls. It meant:\r\n    '{meaning}'");
?FMT(unpackAndInventoryShipmentInCargoBay,"Unload the cargo '{employeeName}'");
?FMT(produceACar,"Employee '{employee}' produce car:{carModel}");
format(X) -> X.

subst(Fmt,[]) -> 
  Fmt;
subst(Fmt,[{K,V}|T]) ->
  Replaced = re:replace(Fmt,"\\{" ++ atom_to_list(K) ++ "\\}",[V],[global,{return,list}]),
    subst(Replaced,T).

stringify(undefined) -> "[?]";
stringify(X) -> X.
