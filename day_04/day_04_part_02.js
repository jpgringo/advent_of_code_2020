const {getDataSet, fieldNames} = require('./dataParser');

function validatePassport(passport) {
  const keys = Object.keys(passport);
  if(keys.length < fieldNames.length - 1
    || (keys.length === fieldNames.length - 1 && keys.includes('cid'))) {
    return false;
  } else {
    let temp;
    for(let key of fieldNames) {
      switch(key) {
        case 'byr' :
          temp = parseInt(passport[key]);
          if(isNaN(temp) || temp < 1920 || 2002 < temp) return false;
          break;
        case 'iyr' :
          temp = parseInt(passport[key]);
          if(isNaN(temp) || temp < 2010 || 2020 < temp) return false;
          break;
        case 'eyr' :
          temp = parseInt(passport[key]);
          if(isNaN(temp) || temp < 2020 || 2030 < temp) return false;
          break;
        case 'hgt' :
          temp = passport[key].match(/(\d+)(cm|in)/i);
          if(!temp) return false;
          const quant = parseInt(temp[1]), unit = temp[2];
          if((unit.toLowerCase() === 'cm' && (quant < 150 || 193 < quant)
          || (unit.toLowerCase() === 'in' && (quant < 59 || 76 < quant)))) return false;
          break;
        case 'hcl':
          if(!/^#[0-9a-f]{6}$/i.test(passport[key])) return false;
          break;
        case 'ecl':
          if(!/^(amb|blu|brn|gry|grn|hzl|oth)$/i.test(passport[key])) return false;
          break;
        case 'pid':
          if(!/^\d{9}$/.test(passport[key])) return false;
          break;
        default:
          break;
      }
    }
  }
  return true;
}

(function main() {
  getDataSet().then(inputData => {
    // console.log(`passports:`, inputData);
    const validPassportCount = inputData.reduce((acc, passport) => {
      const valid = validatePassport(passport);
      return valid ? acc + 1 : acc;
    }, 0);
    console.log(`validPassportCount=${validPassportCount}`);
  });
})();
