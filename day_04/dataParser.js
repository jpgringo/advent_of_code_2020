const fs = require('fs');
const path = require('path');
const readline = require('readline');

const fieldNames = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid', 'cid'];

async function getDataSet() {
  const fileStream = fs.createReadStream(path.join(__dirname, 'input_data.txt'));
  const rl = readline.createInterface({input: fileStream, crlfDelay: Infinity})
  const data = [];
  const fieldRegex = new RegExp('(' + fieldNames.join('|') + '):([^ ]+)', 'gi');
  let passportRecord = {};
  let i = 0;
  for await (const line of rl) {
    i++;
    if (/^\s*$/.test(line)) {
      data.push(passportRecord);
      passportRecord = {};
    } else {
      let match;
      while ((match = fieldRegex.exec(line)) !== null) {
        passportRecord[match[1]] = match[2];
      }
    }
  }
  if (passportRecord !== {}) data.push(passportRecord);
  return data;
}

module.exports.fieldNames = fieldNames.sort();
module.exports.getDataSet = getDataSet;
