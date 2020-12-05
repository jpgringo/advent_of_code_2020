const fs = require('fs');
const path = require('path');

function getDataSet() {
  const foo = fs.readFileSync(path.join(__dirname, '../data_sources/day_02_input_data.txt'), {encoding: "utf-8"});
  const regex = /(\d+)-(\d+)\s+([a-z]):\s*([a-z]+)/gi;
  const entries = [];
  let match;
  while ((match = regex.exec(foo)) !== null) {
    const rec = {};
    ['min', 'max', 'char', 'pw'].forEach((val, i) => {
      rec[val] = match[i + 1];
    });
    entries.push(rec);
  }
  return entries;
}

function validateEntry(entry) {
  const regex = new RegExp(entry.char, 'gi')
  const occurrences = entry.pw.match(regex);
  const count = occurrences ? occurrences.length : 0;
  return entry.min <= count && count <= entry.max;
}

(function main() {
  const inputData = getDataSet();
  const validEntries = inputData.reduce((acc, entry) => {
    return acc + (validateEntry(entry) ? 1 : 0);
  }, 0);
  console.log(`valid entry count:`, validEntries);
})();
