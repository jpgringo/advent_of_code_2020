
const getDataSet = require('./dataParser').getDataSet;

(function main() {
  getDataSet().then(inputData => {
    const tileWidth = inputData[0].length; // just assuming all input lines are the same length
    const treeCount = inputData.reduce((acc, row, i) => {
      const xPos = (i * 3) % tileWidth;
      return row[xPos] === '#' ? acc + 1 : acc;
    }, 0);
    console.log(`treeCount:`, treeCount);
  });
})();
