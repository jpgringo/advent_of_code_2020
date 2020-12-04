const getDataSet = require('./dataParser').getDataSet;

(function main() {
  const traversals = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]];
  getDataSet().then(inputData => {
    const tileWidth = inputData[0].length; // just assuming all input lines are the same length
    let treeProduct = 1;
    traversals.forEach(([xStep, yStep]) => {
      const treeCount = inputData.reduce((acc, row, i) => {
        if(i % yStep !== 0) {
          return acc;
        } else {
          const xPos = (i * xStep) % tileWidth;
          return row[xPos] === '#' ? acc + 1 : acc;
        }
      }, 0);
      treeProduct *= treeCount;
    })
    console.log(`treeProduct=${treeProduct}`);
  });
})();
