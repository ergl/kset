const utils = require("./utils");
const kset_internal = require("../lib/js/src/kset_internal");

function get_index_data(key) {
  return utils.unwrapData(kset_internal.get_index_data(key));
}

function subkeys(key, t) {
  return utils.unwrapJstList(kset_internal.subkeys(key, t));
}

function contents(t) {
  return utils.unwrapJstList(kset_internal.contents(t));
}

function changed(t) {
  // Convert to boolean
  return !!kset_internal.changed(t);
}

function keyToJson(key) {
  const propKeys = Object.keys(key);
  return propKeys.reduce(
    (acc, curr) => {
      const ns = key[curr];
      const nested = Array.isArray(ns) ? [keyToJson(ns)] : ns;
      return Object.assign(acc, { [curr]: nested });
    },
    {}
  );
}

function keyFromJson(ser) {
  let propKeys = Object.keys(ser);
  return propKeys.reduce(
    (acc, curr) => {
      const ns = ser[curr];
      acc[curr] = Array.isArray(ns) ? keyFromJson(ns[0]) : ns;
      return acc;
    },
    []
  );
}

function toJson(t) {
  return contents(t).map(keyToJson);
}

function fromJson(ser) {
  let empt = kset_internal.empty();
  ser.forEach(serKey => {
    kset_internal.add(keyFromJson(serKey), empt);
  });
  // Reset the `changed` flag after adding all keys
  kset_internal.reset(empt);
  return empt;
}

exports.empty = kset_internal.empty;

exports.add = kset_internal.add;
exports.remove = kset_internal.remove;

exports.subkeys = subkeys;

exports.toJson = toJson;
exports.fromJson = fromJson;

exports.changed = changed;
exports.contents = contents;
exports.repr = kset_internal.repr;

exports.int = kset_internal.d_int;
exports.string = kset_internal.d_string;

exports.table = kset_internal.table;
exports.spk = kset_internal.spk;
exports.field = kset_internal.field;

exports.index_key = kset_internal.index_key;
exports.uindex_key = kset_internal.uindex_key;

exports.is_data = kset_internal.is_data;
exports.is_index = kset_internal.is_index;
exports.is_uindex = kset_internal.is_uindex;

exports.get_index_data = get_index_data;
exports.table_fom_key = kset_internal.table_from_key;
exports.table_from_key = kset_internal.table_from_key;
exports.field_from_key = kset_internal.field_from_key;
exports.raw_index_field_value = kset_internal.raw_index_field_value;
