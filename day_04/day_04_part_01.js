const {getDataSet, fieldNames} = require('./dataParser');

(function main() {
  getDataSet().then(inputData => {
    // console.log(`passports:`, inputData);
    const validPassportCount = inputData.reduce((acc, passport) => {
      const keys = Object.keys(passport);
      return keys.length === fieldNames.length
        || (keys.length === fieldNames.length - 1 && !keys.includes('cid'))?
        acc + 1 : acc;
    }, 0);
    console.log(`validPassportCount=${validPassportCount}`);
  });
})();
