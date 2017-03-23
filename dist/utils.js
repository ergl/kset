const List = require("bs-platform/lib/js/list");

const ML_BOTTOM = "$$__ML_BOTTOM__$$";

function safeHead(mlList) {
  try {
    return List.hd(mlList);
  } catch (e) {
    return ML_BOTTOM;
  }
}

function safeTail(mlList) {
  try {
    return List.tl(mlList);
  } catch (e) {
    return [];
  }
}

function toArray(mlList) {
  let remainder = mlList;
  let acc = [];
  let elt;
  while ((elt = safeHead(remainder)) !== ML_BOTTOM) {
    acc.push(elt);
    remainder = safeTail(remainder);
  }

  return acc;
}

function unwrapJstList(jstList) {
  if (jstList === undefined) return jstList;
  return toArray(jstList);
}

// TODO: Extend to allow other kinds of data
function unwrapData(data) {
  switch (data.tag) {
    case 0:
      return data[0];
    default:
      throw new Error(`Can't unwrap ${data}`);
  }
}

module.exports = {
  unwrapData,
  unwrapJstList
};
