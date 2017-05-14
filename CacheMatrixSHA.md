'use strict';

const crypto = require('crypto');
let hash = crypto.createHash('sha1').update('some data to hash').digest('hex');

console.log('hash:', hash);
