open Core.Std
open Yojson.Basic.Util;;
open Unix

(* Class representing a datasource object *)
class data_source (url_string : string) = object(self : 'self)
  method url = url_string;
end ;;

(* Class representing a metro object *)
class metro (data : Yojson.Basic.json) = object(self : 'self)

  (* Member variables of metro object *)
  method code = member "code" data |> to_string;
  method name = member "name" data |> to_string;
  method country = member "country" data |> to_string;
  method continent = member "continent" data |> to_string;

end ;;

(* Class containing our graph of some json data *)
class json_graph data = object(self : 'self)

  (* Read the JSON file *)
  method json = Yojson.Basic.from_file data

  (* Data sources define where data comes from *)
  method data_sources = List.map 
                        (self#json |> member "data sources" |> to_list |> filter_string) 
                        ~f:(fun x -> new data_source x )
  
  (* Populate a list of metro objects *)
  method metros = List.map 
                  (self#json |> member "metros" |> to_list) 
                  ~f:(fun x -> new metro x)

end;;

let cli =
  let graph = new json_graph "map_data.json" in 
  try
    while true do
      print_string "CSAir $ ";
      let cmd = read_line () in 
      printf "%s\n" (cmd);
      List.iter graph#data_sources ~f:(fun x -> printf "%s, " (x#url));
      List.iter graph#metros ~f:(fun x -> printf "%s, " (x#code));
      printf "%s\n" (cmd);
    done
  with End_of_file -> ()

let () = 
  cli 

