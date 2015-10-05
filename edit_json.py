#!/usr/bin/env python
import argparse, json, sys

# Setup argument parser
parser = argparse.ArgumentParser()
parser.add_argument(
    '--file',
    help='File to be changed',
    required=True)
parser.add_argument(
    '--code',
    help='code to be added to new city',
    required=False)
parser.add_argument(
    '--continent',
    help='continent to be added to new city',
    required=False)
parser.add_argument(
    '--coordinates',
    help='coordinates to be added to new city',
    required=False)
parser.add_argument(
    '--country',
    help='country to be added to new city',
    required=False)
parser.add_argument(
    '--name',
    help='name to be added to new city',
    required=False)
parser.add_argument(
    '--population',
    help='population to be added to new city',
    required=False)
parser.add_argument(
    '--region',
    help='region to be added to new city',
    required=False)
parser.add_argument(
    '--timezone',
    help='timezone to be added to new city',
    required=False)
parser.add_argument(
    '--distance',
    help='distance for route to add',
    required=False)
parser.add_argument(
    '--type',
    help='type of change, modify, add or remove',
    required=False)
parser.add_argument(
    '--route',
    help='route to be added or removed',
    required=False)
parser.add_argument(
    '--city',
    help='City to be changed',
    required=False)
parser.add_argument(
    '--element',
    help='Element to be changed',
    required=False)
parser.add_argument(
    '--value',
    help='Value to be changed',
    required=False)
args = vars(parser.parse_args())

def remove_city(data, city):
    """ Removes a given city from json """
    try:
        cities = [r["name"] for r in data["metros"]]
        if(city not in cities):
            raise Exception
        for index, metro in enumerate(data["metros"]):
            if(metro["name"] == city):
                del data["metros"][index]
                break
    except Exception:
        sys.exit(1)

def add_city(data, code, name, country, continent, timezone,
        coordinates, population, region):
    """ Adds a given city to json """
    with open("result", "w") as fh:
        try:
            list_val = coordinates.split(":")
            coord_dict = dict(zip(list_val[0::2], list_val[1::2]))
            for key in coord_dict: coord_dict[key] = int(coord_dict[key])
            metro = {"code": code.replace("_"," "),
                    "continent": continent.replace("_"," "),
                    "coordinates": coord_dict,
                    "country": country.replace("_"," "),
                    "name": name.replace("_"," "),
                    "population": int(population),
                    "region": int(region),
                    "timezone": float(timezone)}
            data["metros"].append(metro)
        except Exception as e:
            fh.write("{}".format(e.strerror))
            sys.exit(1)

def add_route(data, distance, route_before):
    """ Adds a given route to json """
    try:
        src, dst = route_before.split("-")
        route = {"distance": int(distance), "ports": [src, dst]}
        data["routes"].append(route)
    except Exception:
        sys.exit(1)

def remove_route(data, route):
    """ Removes a given route from json """
    try:
        routes = [r["ports"] for r in data["routes"]]
        if(route.split("-") not in routes):
            raise Exception
        src, dst = route.split("-")
        for index, route in enumerate(data["routes"]):
            if(route["ports"][0] == src and route["ports"][1] == dst):
                del data["routes"][index]
                break
    except Exception:
        sys.exit(1)

def modify_city(data, city, element, value):
    """ Modifies a given element in the json file """
    try:
        for metro in data["metros"]:
            if(metro["name"] == city):
                if(element in ["code", "name", "country", "continent"]):
                    metro[element] = value.replace("_", " ")
                elif(element in ["population", "region"]):
                    metro[element] = int(value)
                elif(element == "timezone"):
                    metro[element] = float(value)
                elif(element == "coordinates"):
                    list_val = value.split(":")
                    dict_val  = dict(zip(list_val[0::2], list_val[1::2]))
                    for key in dict_val: dict_val[key] = int(dict_val[key])
                    metro[element] = dict_val
    except Exception:
        sys.exit(1)

def read_and_write():
    """ Read and write json to a file """
    with open(args["file"], "r") as jsonFile:
        data = json.load(jsonFile)

    if(args["type"] == "modify"):
        if(args["city"] is not None):
            if(args["element"] is None or args["value"] is None):
                sys.exit(1)
            modify_city(data, args["city"], args["element"], args["value"])
    elif(args["type"] == "add"):
        if(args["distance"] is not None and args["route"] is not None):
            add_route(data, args["distance"], args["route"])
        if(args["code"] is not None and args["continent"] is not None
                and args["coordinates"] is not None and args["country"] is not None
                and args["name"] is not None and args["population"] is not None
                and args["region"] is not None and args["timezone"] is not None):
            add_city(data, args["code"], args["name"], args["country"],
                    args["continent"], args["timezone"], args["coordinates"],
                    args["population"], args["region"])
    elif(args["type"] == "remove"):
        if(args["city"] is not None):
            remove_city(data, args["city"])
        if(args["route"] is not None):
            remove_route(data, args["route"])

    with open(args["file"], "w") as jsonFile:
        jsonFile.write(json.dumps(data , indent=4, sort_keys=True))

def main():
    read_and_write()

if __name__ == '__main__':
    main()
