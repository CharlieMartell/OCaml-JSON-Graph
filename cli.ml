open Core.Std
open Yojson.Basic.Util;;
open Unix

let help = "Usage:\n"
         ^ "    city <city> <data_element> (* Gets <data_element> of a given <city>\n"
         ^ "                                <data_element> -> (code, name, country, \n"
         ^ "                                continent, timezone, coordinates, *)\n"
         ^ "                                population, region, all) *)\n"
         ^ "    network <stats|data> <stat_element> (* <data> -> Gets all cities CSAir flies to\n"
         ^ "                                         <stat_element> -> (TBD) *)\n"

(* Class representing a datasource object *)
class data_source (url_string : string) = object(self : 'self)
  method url = url_string;
end

(* Define coordinate type *)
class coordinates (data : Yojson.Basic.json) = object(self : 'self)

  method get_dir dir = match member dir data with
                  | `Null -> 0
                  | _ -> member dir data |> to_int;

  (* Define various coordinates to parse from json*)
  method north  = self#get_dir "N"
  method south  = self#get_dir "S"
  method west   = self#get_dir "W"
  method east   = self#get_dir "E"

  (* Gets a string representation of class *)
  method string_of = "{ North: " ^ Int.to_string self#north
                   ^ " South: " ^ Int.to_string self#south 
                   ^ " West: " ^ Int.to_string self#west 
                   ^ " East: " ^ Int.to_string self#east ^ " }"
end

(* Class representing a metro object *)
class metro (data : Yojson.Basic.json) = object(self : 'self)

  (* Member variables of metro object *)
  method code        = member "code"        data |> to_string;
  method name        = member "name"        data |> to_string;
  method country     = member "country"     data |> to_string;
  method continent   = member "continent"   data |> to_string;
  method timezone    = member "timezone"    data |> to_float;
  method coordinates = new coordinates (member "coordinates" data);
  method population  = member "population"  data |> to_int;
  method region      = member "region"      data |> to_int;

end

(* Class representing a route object *)
class route (data : Yojson.Basic.json) = object(self : 'self)

  (* Member variables of route object *)
  method ports    = member "ports"    data |> to_list;
  method distance = member "distance" data |> to_string;

end

(* Class containing our graph of some json data *)
class json_graph data = object(self : 'self)

  (* Read the JSON file *)
  method json = Yojson.Basic.from_file data

  (* Data sources define where data comes from *)
  method data_sources = List.map 
                        (self#json |> member "data sources" |> to_list |> filter_string) 
                        ~f:(fun x -> new data_source x)
  
  (* Populate a list of metro objects *)
  method metros = List.map 
                  (self#json |> member "metros" |> to_list) 
                  ~f:(fun x -> new metro x)

  (* Populate a list of route objects *)
  method routes = List.map 
                  (self#json |> member "routes" |> to_list) 
                  ~f:(fun x -> new route x)
end;;

(* Globals *)
let graph = new json_graph "map_data.json"

(* Splits command argument words referenced from 
 *  Unix system programming in OCaml *)
let split_words words =
 let rec skip_blanks index =
   if index < String.length words && words.[index] = ' '
   then skip_blanks (index+1)
   else index in
 let rec split start index =
   if index >= String.length words then
     [String.sub words start (index-start)]
   else if words.[index] = ' ' then
     let index_two = skip_blanks index in
     String.sub words start (index-start) :: split index_two index_two
   else
     split start (index+1) in
 Array.of_list (split 0 0);;

(* Parses city as first element in cli *)
let parse_city cmds= 
  if 3 >  Array.length cmds then help else
  let city = cmds.(1) in 
  let element = cmds.(2) in 
  let city_data = List.find graph#metros ~f:(fun x -> x#name = city) in 
  match city_data with 
  | None -> "City not found!"
  | Some x -> match element with 
              | "code"        -> x#code
              | "name"        -> x#name
              | "country"     -> x#country
              | "continent"   -> x#continent
              | "timezone"    -> (Float.to_string x#timezone)
              | "coordinates" -> x#coordinates#string_of
              | "population"  -> (Int.to_string x#population)
              | "region"      -> (Int.to_string x#region)
              | _             -> "Not yet Implemented"

(* Parses network as first element in cli *)
let parse_network cmds = 
  if 2 >  Array.length cmds then help else
  match cmds.(1) with
  | "stats" -> "Not yet Implemented"
  | _       -> (String.concat ~sep:", " (List.map graph#metros ~f:(fun x -> x#name)))

(* Parse array of commands *)
let parse_cmd cmds = 
  if 0 = Array.length cmds then help else
  match cmds.(0) with
  | "network" -> parse_network cmds 
  | "city"    -> parse_city cmds
  | _         -> help

(* Main cli loop *)
let cli =
  try
    while true do
      print_string "CSAir $ ";
      let cmd = read_line () in 
      let cmds = split_words cmd in 
      let response = parse_cmd cmds in 
      printf "%s\n" (response);
      (* DEBUG: List.iter graph#routes ~f:(fun x -> List.iter x#ports (fun y -> printf "%s, " (y |> to_string)))*) done
  with End_of_file -> ()

let () = 
  cli 

