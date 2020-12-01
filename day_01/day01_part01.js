const fs = require('fs');
const path = require('path');

function getDataSet() {
  const foo = fs.readFileSync(path.join(__dirname, 'input_data.txt'), {encoding: "utf-8"});
  return foo.match(/\d+/g).map(input => parseInt(input));
}

function getMatchingPair(inputNumbers, targetSum) {
  if(inputNumbers.length === 0) throw new Error('no matching numbers');
  const candidate = inputNumbers[0];
  const rest = inputNumbers.slice(1);
  for (let i = 0; i < rest.length; i++) {
    const test = rest[i];
    if(candidate + test === targetSum) {
      return [candidate, test];
    }
  }
  return getMatchingPair(rest, targetSum);
}

(function main() {
  const inputNumbers = getDataSet();
  console.log(`inputNumbers:`, inputNumbers);
  const matchingPair = getMatchingPair(inputNumbers, 2020);
  console.log(`matchingPair:`, matchingPair);
  console.log('product:', matchingPair[0] * matchingPair[1]);
})();

