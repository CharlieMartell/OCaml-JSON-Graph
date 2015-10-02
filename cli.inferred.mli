module JsonGraph :
  sig
    val help : string
    class data_source : string -> object method url : string end
    class coordinates :
      Yojson.Basic.json ->
      object
        method east : int
        method get_dir : string -> int
        method north : int
        method south : int
        method string_of : string
        method west : int
      end
    class metro :
      Yojson.Basic.json ->
      object
        method code : string
        method continent : string
        method coordinates : coordinates
        method country : string
        method name : string
        method population : int
        method region : int
        method timezone : float
      end
    class route :
      Yojson.Basic.json ->
      object
        val ports : string list
        method distance : int
        method dst_metro : string
        method ports : string list
        method src_metro : string
      end
    class json_graph :
      string ->
      object
        method average_distance : string
        method average_size : string
        method biggest_city : string
        method continents : string
        method data_sources : data_source list
        method hub_cities : string
        method json : Yojson.Basic.json
        method longest_distance : string
        method metros : metro list
        method routes : route list
        method shortest_distance : string
        method smallest_city : string
      end
    val graph : json_graph
    val split_words : string -> string array
    val parse_city : string array -> string
    val parse_stat_elements : string array -> string
    val parse_network : string array -> string
    val parse_flight : string array -> string
    val parse_cmd : string array -> string
    val cli : unit
  end
