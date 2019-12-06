let x = [...Array(20).keys()]
console.log([...Array(20).keys()].filter(e => {return e % 2 == 0}).filter(e => {return e > 10}).map(e => {return e * 2}));
