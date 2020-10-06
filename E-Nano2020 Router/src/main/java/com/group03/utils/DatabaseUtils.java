/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: DatabaseUtils.java
 * Description:
 *    Utilities for database access using MongoDB.
 * Authors:
 * - David Alberto Guevara Sánchez
 *   402450355
 * - Joy Bonilla Fley
 *   402360421
 * - Jose Barrantes Araya
 *   207600954
 * - Natalia Solano Azofeifa
 *   117290958
 * - Luis David Villalobos Gonzalez
 *   117540697
 * Group: 03
 * Schedule: 10am
 * Date of modification: 2020-10-05
 */

package com.group03.utils;

import java.util.ArrayList;
import java.util.stream.Collectors;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;

import org.bson.Document;
import org.json.JSONArray;
import org.json.JSONObject;

public class DatabaseUtils {
  private static String uri = "mongodb+srv://admin:x38qoj4KrQmxQRm0@database.cvahn.mongodb.net/";

  public static JSONObject getInfo() {
    var client = new MongoClient(new MongoClientURI(uri)); 
    var database = client.getDatabase("ENANO2020");
    var info_collection = database.getCollection("INFO");
    var team_collection = database.getCollection("TEAM");

    var info_data = info_collection.find().first();
    var info_json = info_data.toJson();
    var info_object = new JSONObject(info_json);

    var team_list = team_collection.find().into(new ArrayList<Document>())
      .stream()
      .map(Document::toJson)
      .collect(Collectors.toList());
    var team_object = team_list.stream().map(JSONObject::new).collect(JSONArray::new, JSONArray::put, JSONArray::put);

    var aux = new JSONObject();
    aux.put("code", info_object.getString("code"));
    info_object.remove("code");
    aux.put("members", team_object);

    info_object.put("team", aux);
    return info_object;
  }

}

