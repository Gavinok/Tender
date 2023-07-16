(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __commonJS = (cb, mod3) => function __require() {
    return mod3 || (0, cb[__getOwnPropNames(cb)[0]])((mod3 = { exports: {} }).exports, mod3), mod3.exports;
  };
  var __copyProps = (to3, from3, except3, desc) => {
    if (from3 && typeof from3 === "object" || typeof from3 === "function") {
      for (let key of __getOwnPropNames(from3))
        if (!__hasOwnProp.call(to3, key) && key !== except3)
          __defProp(to3, key, { get: () => from3[key], enumerable: !(desc = __getOwnPropDesc(from3, key)) || desc.enumerable });
    }
    return to3;
  };
  var __toESM = (mod3, isNodeMode, target) => (target = mod3 != null ? __create(__getProtoOf(mod3)) : {}, __copyProps(
    isNodeMode || !mod3 || !mod3.__esModule ? __defProp(target, "default", { value: mod3, enumerable: true }) : target,
    mod3
  ));

  // ../../../../../node_modules/big-integer/BigInteger.js
  var require_BigInteger = __commonJS({
    "../../../../../node_modules/big-integer/BigInteger.js"(exports, module) {
      var bigInt2 = function(undefined2) {
        "use strict";
        var BASE = 1e7, LOG_BASE = 7, MAX_INT = 9007199254740992, MAX_INT_ARR = smallToArray(MAX_INT), DEFAULT_ALPHABET = "0123456789abcdefghijklmnopqrstuvwxyz";
        var supportsNativeBigInt = typeof BigInt === "function";
        function Integer(v, radix, alphabet, caseSensitive) {
          if (typeof v === "undefined")
            return Integer[0];
          if (typeof radix !== "undefined")
            return +radix === 10 && !alphabet ? parseValue(v) : parseBase(v, radix, alphabet, caseSensitive);
          return parseValue(v);
        }
        function BigInteger(value, sign2) {
          this.value = value;
          this.sign = sign2;
          this.isSmall = false;
        }
        BigInteger.prototype = Object.create(Integer.prototype);
        function SmallInteger(value) {
          this.value = value;
          this.sign = value < 0;
          this.isSmall = true;
        }
        SmallInteger.prototype = Object.create(Integer.prototype);
        function NativeBigInt(value) {
          this.value = value;
        }
        NativeBigInt.prototype = Object.create(Integer.prototype);
        function isPrecise(n) {
          return -MAX_INT < n && n < MAX_INT;
        }
        function smallToArray(n) {
          if (n < 1e7)
            return [n];
          if (n < 1e14)
            return [n % 1e7, Math.floor(n / 1e7)];
          return [n % 1e7, Math.floor(n / 1e7) % 1e7, Math.floor(n / 1e14)];
        }
        function arrayToSmall(arr) {
          trim2(arr);
          var length4 = arr.length;
          if (length4 < 4 && compareAbs(arr, MAX_INT_ARR) < 0) {
            switch (length4) {
              case 0:
                return 0;
              case 1:
                return arr[0];
              case 2:
                return arr[0] + arr[1] * BASE;
              default:
                return arr[0] + (arr[1] + arr[2] * BASE) * BASE;
            }
          }
          return arr;
        }
        function trim2(v) {
          var i2 = v.length;
          while (v[--i2] === 0)
            ;
          v.length = i2 + 1;
        }
        function createArray(length4) {
          var x = new Array(length4);
          var i2 = -1;
          while (++i2 < length4) {
            x[i2] = 0;
          }
          return x;
        }
        function truncate(n) {
          if (n > 0)
            return Math.floor(n);
          return Math.ceil(n);
        }
        function add2(a, b) {
          var l_a = a.length, l_b = b.length, r = new Array(l_a), carry = 0, base = BASE, sum2, i2;
          for (i2 = 0; i2 < l_b; i2++) {
            sum2 = a[i2] + b[i2] + carry;
            carry = sum2 >= base ? 1 : 0;
            r[i2] = sum2 - carry * base;
          }
          while (i2 < l_a) {
            sum2 = a[i2] + carry;
            carry = sum2 === base ? 1 : 0;
            r[i2++] = sum2 - carry * base;
          }
          if (carry > 0)
            r.push(carry);
          return r;
        }
        function addAny(a, b) {
          if (a.length >= b.length)
            return add2(a, b);
          return add2(b, a);
        }
        function addSmall(a, carry) {
          var l = a.length, r = new Array(l), base = BASE, sum2, i2;
          for (i2 = 0; i2 < l; i2++) {
            sum2 = a[i2] - base + carry;
            carry = Math.floor(sum2 / base);
            r[i2] = sum2 - carry * base;
            carry += 1;
          }
          while (carry > 0) {
            r[i2++] = carry % base;
            carry = Math.floor(carry / base);
          }
          return r;
        }
        BigInteger.prototype.add = function(v) {
          var n = parseValue(v);
          if (this.sign !== n.sign) {
            return this.subtract(n.negate());
          }
          var a = this.value, b = n.value;
          if (n.isSmall) {
            return new BigInteger(addSmall(a, Math.abs(b)), this.sign);
          }
          return new BigInteger(addAny(a, b), this.sign);
        };
        BigInteger.prototype.plus = BigInteger.prototype.add;
        SmallInteger.prototype.add = function(v) {
          var n = parseValue(v);
          var a = this.value;
          if (a < 0 !== n.sign) {
            return this.subtract(n.negate());
          }
          var b = n.value;
          if (n.isSmall) {
            if (isPrecise(a + b))
              return new SmallInteger(a + b);
            b = smallToArray(Math.abs(b));
          }
          return new BigInteger(addSmall(b, Math.abs(a)), a < 0);
        };
        SmallInteger.prototype.plus = SmallInteger.prototype.add;
        NativeBigInt.prototype.add = function(v) {
          return new NativeBigInt(this.value + parseValue(v).value);
        };
        NativeBigInt.prototype.plus = NativeBigInt.prototype.add;
        function subtract(a, b) {
          var a_l = a.length, b_l = b.length, r = new Array(a_l), borrow = 0, base = BASE, i2, difference2;
          for (i2 = 0; i2 < b_l; i2++) {
            difference2 = a[i2] - borrow - b[i2];
            if (difference2 < 0) {
              difference2 += base;
              borrow = 1;
            } else
              borrow = 0;
            r[i2] = difference2;
          }
          for (i2 = b_l; i2 < a_l; i2++) {
            difference2 = a[i2] - borrow;
            if (difference2 < 0)
              difference2 += base;
            else {
              r[i2++] = difference2;
              break;
            }
            r[i2] = difference2;
          }
          for (; i2 < a_l; i2++) {
            r[i2] = a[i2];
          }
          trim2(r);
          return r;
        }
        function subtractAny(a, b, sign2) {
          var value;
          if (compareAbs(a, b) >= 0) {
            value = subtract(a, b);
          } else {
            value = subtract(b, a);
            sign2 = !sign2;
          }
          value = arrayToSmall(value);
          if (typeof value === "number") {
            if (sign2)
              value = -value;
            return new SmallInteger(value);
          }
          return new BigInteger(value, sign2);
        }
        function subtractSmall(a, b, sign2) {
          var l = a.length, r = new Array(l), carry = -b, base = BASE, i2, difference2;
          for (i2 = 0; i2 < l; i2++) {
            difference2 = a[i2] + carry;
            carry = Math.floor(difference2 / base);
            difference2 %= base;
            r[i2] = difference2 < 0 ? difference2 + base : difference2;
          }
          r = arrayToSmall(r);
          if (typeof r === "number") {
            if (sign2)
              r = -r;
            return new SmallInteger(r);
          }
          return new BigInteger(r, sign2);
        }
        BigInteger.prototype.subtract = function(v) {
          var n = parseValue(v);
          if (this.sign !== n.sign) {
            return this.add(n.negate());
          }
          var a = this.value, b = n.value;
          if (n.isSmall)
            return subtractSmall(a, Math.abs(b), this.sign);
          return subtractAny(a, b, this.sign);
        };
        BigInteger.prototype.minus = BigInteger.prototype.subtract;
        SmallInteger.prototype.subtract = function(v) {
          var n = parseValue(v);
          var a = this.value;
          if (a < 0 !== n.sign) {
            return this.add(n.negate());
          }
          var b = n.value;
          if (n.isSmall) {
            return new SmallInteger(a - b);
          }
          return subtractSmall(b, Math.abs(a), a >= 0);
        };
        SmallInteger.prototype.minus = SmallInteger.prototype.subtract;
        NativeBigInt.prototype.subtract = function(v) {
          return new NativeBigInt(this.value - parseValue(v).value);
        };
        NativeBigInt.prototype.minus = NativeBigInt.prototype.subtract;
        BigInteger.prototype.negate = function() {
          return new BigInteger(this.value, !this.sign);
        };
        SmallInteger.prototype.negate = function() {
          var sign2 = this.sign;
          var small = new SmallInteger(-this.value);
          small.sign = !sign2;
          return small;
        };
        NativeBigInt.prototype.negate = function() {
          return new NativeBigInt(-this.value);
        };
        BigInteger.prototype.abs = function() {
          return new BigInteger(this.value, false);
        };
        SmallInteger.prototype.abs = function() {
          return new SmallInteger(Math.abs(this.value));
        };
        NativeBigInt.prototype.abs = function() {
          return new NativeBigInt(this.value >= 0 ? this.value : -this.value);
        };
        function multiplyLong(a, b) {
          var a_l = a.length, b_l = b.length, l = a_l + b_l, r = createArray(l), base = BASE, product2, carry, i2, a_i, b_j;
          for (i2 = 0; i2 < a_l; ++i2) {
            a_i = a[i2];
            for (var j = 0; j < b_l; ++j) {
              b_j = b[j];
              product2 = a_i * b_j + r[i2 + j];
              carry = Math.floor(product2 / base);
              r[i2 + j] = product2 - carry * base;
              r[i2 + j + 1] += carry;
            }
          }
          trim2(r);
          return r;
        }
        function multiplySmall(a, b) {
          var l = a.length, r = new Array(l), base = BASE, carry = 0, product2, i2;
          for (i2 = 0; i2 < l; i2++) {
            product2 = a[i2] * b + carry;
            carry = Math.floor(product2 / base);
            r[i2] = product2 - carry * base;
          }
          while (carry > 0) {
            r[i2++] = carry % base;
            carry = Math.floor(carry / base);
          }
          return r;
        }
        function shiftLeft(x, n) {
          var r = [];
          while (n-- > 0)
            r.push(0);
          return r.concat(x);
        }
        function multiplyKaratsuba(x, y) {
          var n = Math.max(x.length, y.length);
          if (n <= 30)
            return multiplyLong(x, y);
          n = Math.ceil(n / 2);
          var b = x.slice(n), a = x.slice(0, n), d = y.slice(n), c = y.slice(0, n);
          var ac = multiplyKaratsuba(a, c), bd = multiplyKaratsuba(b, d), abcd = multiplyKaratsuba(addAny(a, b), addAny(c, d));
          var product2 = addAny(addAny(ac, shiftLeft(subtract(subtract(abcd, ac), bd), n)), shiftLeft(bd, 2 * n));
          trim2(product2);
          return product2;
        }
        function useKaratsuba(l1, l2) {
          return -0.012 * l1 - 0.012 * l2 + 15e-6 * l1 * l2 > 0;
        }
        BigInteger.prototype.multiply = function(v) {
          var n = parseValue(v), a = this.value, b = n.value, sign2 = this.sign !== n.sign, abs3;
          if (n.isSmall) {
            if (b === 0)
              return Integer[0];
            if (b === 1)
              return this;
            if (b === -1)
              return this.negate();
            abs3 = Math.abs(b);
            if (abs3 < BASE) {
              return new BigInteger(multiplySmall(a, abs3), sign2);
            }
            b = smallToArray(abs3);
          }
          if (useKaratsuba(a.length, b.length))
            return new BigInteger(multiplyKaratsuba(a, b), sign2);
          return new BigInteger(multiplyLong(a, b), sign2);
        };
        BigInteger.prototype.times = BigInteger.prototype.multiply;
        function multiplySmallAndArray(a, b, sign2) {
          if (a < BASE) {
            return new BigInteger(multiplySmall(b, a), sign2);
          }
          return new BigInteger(multiplyLong(b, smallToArray(a)), sign2);
        }
        SmallInteger.prototype._multiplyBySmall = function(a) {
          if (isPrecise(a.value * this.value)) {
            return new SmallInteger(a.value * this.value);
          }
          return multiplySmallAndArray(Math.abs(a.value), smallToArray(Math.abs(this.value)), this.sign !== a.sign);
        };
        BigInteger.prototype._multiplyBySmall = function(a) {
          if (a.value === 0)
            return Integer[0];
          if (a.value === 1)
            return this;
          if (a.value === -1)
            return this.negate();
          return multiplySmallAndArray(Math.abs(a.value), this.value, this.sign !== a.sign);
        };
        SmallInteger.prototype.multiply = function(v) {
          return parseValue(v)._multiplyBySmall(this);
        };
        SmallInteger.prototype.times = SmallInteger.prototype.multiply;
        NativeBigInt.prototype.multiply = function(v) {
          return new NativeBigInt(this.value * parseValue(v).value);
        };
        NativeBigInt.prototype.times = NativeBigInt.prototype.multiply;
        function square(a) {
          var l = a.length, r = createArray(l + l), base = BASE, product2, carry, i2, a_i, a_j;
          for (i2 = 0; i2 < l; i2++) {
            a_i = a[i2];
            carry = 0 - a_i * a_i;
            for (var j = i2; j < l; j++) {
              a_j = a[j];
              product2 = 2 * (a_i * a_j) + r[i2 + j] + carry;
              carry = Math.floor(product2 / base);
              r[i2 + j] = product2 - carry * base;
            }
            r[i2 + l] = carry;
          }
          trim2(r);
          return r;
        }
        BigInteger.prototype.square = function() {
          return new BigInteger(square(this.value), false);
        };
        SmallInteger.prototype.square = function() {
          var value = this.value * this.value;
          if (isPrecise(value))
            return new SmallInteger(value);
          return new BigInteger(square(smallToArray(Math.abs(this.value))), false);
        };
        NativeBigInt.prototype.square = function(v) {
          return new NativeBigInt(this.value * this.value);
        };
        function divMod1(a, b) {
          var a_l = a.length, b_l = b.length, base = BASE, result = createArray(b.length), divisorMostSignificantDigit = b[b_l - 1], lambda = Math.ceil(base / (2 * divisorMostSignificantDigit)), remainder2 = multiplySmall(a, lambda), divisor = multiplySmall(b, lambda), quotientDigit, shift, carry, borrow, i2, l, q;
          if (remainder2.length <= a_l)
            remainder2.push(0);
          divisor.push(0);
          divisorMostSignificantDigit = divisor[b_l - 1];
          for (shift = a_l - b_l; shift >= 0; shift--) {
            quotientDigit = base - 1;
            if (remainder2[shift + b_l] !== divisorMostSignificantDigit) {
              quotientDigit = Math.floor((remainder2[shift + b_l] * base + remainder2[shift + b_l - 1]) / divisorMostSignificantDigit);
            }
            carry = 0;
            borrow = 0;
            l = divisor.length;
            for (i2 = 0; i2 < l; i2++) {
              carry += quotientDigit * divisor[i2];
              q = Math.floor(carry / base);
              borrow += remainder2[shift + i2] - (carry - q * base);
              carry = q;
              if (borrow < 0) {
                remainder2[shift + i2] = borrow + base;
                borrow = -1;
              } else {
                remainder2[shift + i2] = borrow;
                borrow = 0;
              }
            }
            while (borrow !== 0) {
              quotientDigit -= 1;
              carry = 0;
              for (i2 = 0; i2 < l; i2++) {
                carry += remainder2[shift + i2] - base + divisor[i2];
                if (carry < 0) {
                  remainder2[shift + i2] = carry + base;
                  carry = 0;
                } else {
                  remainder2[shift + i2] = carry;
                  carry = 1;
                }
              }
              borrow += carry;
            }
            result[shift] = quotientDigit;
          }
          remainder2 = divModSmall(remainder2, lambda)[0];
          return [arrayToSmall(result), arrayToSmall(remainder2)];
        }
        function divMod2(a, b) {
          var a_l = a.length, b_l = b.length, result = [], part = [], base = BASE, guess, xlen, highx, highy, check;
          while (a_l) {
            part.unshift(a[--a_l]);
            trim2(part);
            if (compareAbs(part, b) < 0) {
              result.push(0);
              continue;
            }
            xlen = part.length;
            highx = part[xlen - 1] * base + part[xlen - 2];
            highy = b[b_l - 1] * base + b[b_l - 2];
            if (xlen > b_l) {
              highx = (highx + 1) * base;
            }
            guess = Math.ceil(highx / highy);
            do {
              check = multiplySmall(b, guess);
              if (compareAbs(check, part) <= 0)
                break;
              guess--;
            } while (guess);
            result.push(guess);
            part = subtract(part, check);
          }
          result.reverse();
          return [arrayToSmall(result), arrayToSmall(part)];
        }
        function divModSmall(value, lambda) {
          var length4 = value.length, quotient = createArray(length4), base = BASE, i2, q, remainder2, divisor;
          remainder2 = 0;
          for (i2 = length4 - 1; i2 >= 0; --i2) {
            divisor = remainder2 * base + value[i2];
            q = truncate(divisor / lambda);
            remainder2 = divisor - q * lambda;
            quotient[i2] = q | 0;
          }
          return [quotient, remainder2 | 0];
        }
        function divModAny(self, v) {
          var value, n = parseValue(v);
          if (supportsNativeBigInt) {
            return [new NativeBigInt(self.value / n.value), new NativeBigInt(self.value % n.value)];
          }
          var a = self.value, b = n.value;
          var quotient;
          if (b === 0)
            throw new Error("Cannot divide by zero");
          if (self.isSmall) {
            if (n.isSmall) {
              return [new SmallInteger(truncate(a / b)), new SmallInteger(a % b)];
            }
            return [Integer[0], self];
          }
          if (n.isSmall) {
            if (b === 1)
              return [self, Integer[0]];
            if (b == -1)
              return [self.negate(), Integer[0]];
            var abs3 = Math.abs(b);
            if (abs3 < BASE) {
              value = divModSmall(a, abs3);
              quotient = arrayToSmall(value[0]);
              var remainder2 = value[1];
              if (self.sign)
                remainder2 = -remainder2;
              if (typeof quotient === "number") {
                if (self.sign !== n.sign)
                  quotient = -quotient;
                return [new SmallInteger(quotient), new SmallInteger(remainder2)];
              }
              return [new BigInteger(quotient, self.sign !== n.sign), new SmallInteger(remainder2)];
            }
            b = smallToArray(abs3);
          }
          var comparison = compareAbs(a, b);
          if (comparison === -1)
            return [Integer[0], self];
          if (comparison === 0)
            return [Integer[self.sign === n.sign ? 1 : -1], Integer[0]];
          if (a.length + b.length <= 200)
            value = divMod1(a, b);
          else
            value = divMod2(a, b);
          quotient = value[0];
          var qSign = self.sign !== n.sign, mod3 = value[1], mSign = self.sign;
          if (typeof quotient === "number") {
            if (qSign)
              quotient = -quotient;
            quotient = new SmallInteger(quotient);
          } else
            quotient = new BigInteger(quotient, qSign);
          if (typeof mod3 === "number") {
            if (mSign)
              mod3 = -mod3;
            mod3 = new SmallInteger(mod3);
          } else
            mod3 = new BigInteger(mod3, mSign);
          return [quotient, mod3];
        }
        BigInteger.prototype.divmod = function(v) {
          var result = divModAny(this, v);
          return {
            quotient: result[0],
            remainder: result[1]
          };
        };
        NativeBigInt.prototype.divmod = SmallInteger.prototype.divmod = BigInteger.prototype.divmod;
        BigInteger.prototype.divide = function(v) {
          return divModAny(this, v)[0];
        };
        NativeBigInt.prototype.over = NativeBigInt.prototype.divide = function(v) {
          return new NativeBigInt(this.value / parseValue(v).value);
        };
        SmallInteger.prototype.over = SmallInteger.prototype.divide = BigInteger.prototype.over = BigInteger.prototype.divide;
        BigInteger.prototype.mod = function(v) {
          return divModAny(this, v)[1];
        };
        NativeBigInt.prototype.mod = NativeBigInt.prototype.remainder = function(v) {
          return new NativeBigInt(this.value % parseValue(v).value);
        };
        SmallInteger.prototype.remainder = SmallInteger.prototype.mod = BigInteger.prototype.remainder = BigInteger.prototype.mod;
        BigInteger.prototype.pow = function(v) {
          var n = parseValue(v), a = this.value, b = n.value, value, x, y;
          if (b === 0)
            return Integer[1];
          if (a === 0)
            return Integer[0];
          if (a === 1)
            return Integer[1];
          if (a === -1)
            return n.isEven() ? Integer[1] : Integer[-1];
          if (n.sign) {
            return Integer[0];
          }
          if (!n.isSmall)
            throw new Error("The exponent " + n.toString() + " is too large.");
          if (this.isSmall) {
            if (isPrecise(value = Math.pow(a, b)))
              return new SmallInteger(truncate(value));
          }
          x = this;
          y = Integer[1];
          while (true) {
            if (b & true) {
              y = y.times(x);
              --b;
            }
            if (b === 0)
              break;
            b /= 2;
            x = x.square();
          }
          return y;
        };
        SmallInteger.prototype.pow = BigInteger.prototype.pow;
        NativeBigInt.prototype.pow = function(v) {
          var n = parseValue(v);
          var a = this.value, b = n.value;
          var _0 = BigInt(0), _1 = BigInt(1), _2 = BigInt(2);
          if (b === _0)
            return Integer[1];
          if (a === _0)
            return Integer[0];
          if (a === _1)
            return Integer[1];
          if (a === BigInt(-1))
            return n.isEven() ? Integer[1] : Integer[-1];
          if (n.isNegative())
            return new NativeBigInt(_0);
          var x = this;
          var y = Integer[1];
          while (true) {
            if ((b & _1) === _1) {
              y = y.times(x);
              --b;
            }
            if (b === _0)
              break;
            b /= _2;
            x = x.square();
          }
          return y;
        };
        BigInteger.prototype.modPow = function(exp2, mod3) {
          exp2 = parseValue(exp2);
          mod3 = parseValue(mod3);
          if (mod3.isZero())
            throw new Error("Cannot take modPow with modulus 0");
          var r = Integer[1], base = this.mod(mod3);
          if (exp2.isNegative()) {
            exp2 = exp2.multiply(Integer[-1]);
            base = base.modInv(mod3);
          }
          while (exp2.isPositive()) {
            if (base.isZero())
              return Integer[0];
            if (exp2.isOdd())
              r = r.multiply(base).mod(mod3);
            exp2 = exp2.divide(2);
            base = base.square().mod(mod3);
          }
          return r;
        };
        NativeBigInt.prototype.modPow = SmallInteger.prototype.modPow = BigInteger.prototype.modPow;
        function compareAbs(a, b) {
          if (a.length !== b.length) {
            return a.length > b.length ? 1 : -1;
          }
          for (var i2 = a.length - 1; i2 >= 0; i2--) {
            if (a[i2] !== b[i2])
              return a[i2] > b[i2] ? 1 : -1;
          }
          return 0;
        }
        BigInteger.prototype.compareAbs = function(v) {
          var n = parseValue(v), a = this.value, b = n.value;
          if (n.isSmall)
            return 1;
          return compareAbs(a, b);
        };
        SmallInteger.prototype.compareAbs = function(v) {
          var n = parseValue(v), a = Math.abs(this.value), b = n.value;
          if (n.isSmall) {
            b = Math.abs(b);
            return a === b ? 0 : a > b ? 1 : -1;
          }
          return -1;
        };
        NativeBigInt.prototype.compareAbs = function(v) {
          var a = this.value;
          var b = parseValue(v).value;
          a = a >= 0 ? a : -a;
          b = b >= 0 ? b : -b;
          return a === b ? 0 : a > b ? 1 : -1;
        };
        BigInteger.prototype.compare = function(v) {
          if (v === Infinity) {
            return -1;
          }
          if (v === -Infinity) {
            return 1;
          }
          var n = parseValue(v), a = this.value, b = n.value;
          if (this.sign !== n.sign) {
            return n.sign ? 1 : -1;
          }
          if (n.isSmall) {
            return this.sign ? -1 : 1;
          }
          return compareAbs(a, b) * (this.sign ? -1 : 1);
        };
        BigInteger.prototype.compareTo = BigInteger.prototype.compare;
        SmallInteger.prototype.compare = function(v) {
          if (v === Infinity) {
            return -1;
          }
          if (v === -Infinity) {
            return 1;
          }
          var n = parseValue(v), a = this.value, b = n.value;
          if (n.isSmall) {
            return a == b ? 0 : a > b ? 1 : -1;
          }
          if (a < 0 !== n.sign) {
            return a < 0 ? -1 : 1;
          }
          return a < 0 ? 1 : -1;
        };
        SmallInteger.prototype.compareTo = SmallInteger.prototype.compare;
        NativeBigInt.prototype.compare = function(v) {
          if (v === Infinity) {
            return -1;
          }
          if (v === -Infinity) {
            return 1;
          }
          var a = this.value;
          var b = parseValue(v).value;
          return a === b ? 0 : a > b ? 1 : -1;
        };
        NativeBigInt.prototype.compareTo = NativeBigInt.prototype.compare;
        BigInteger.prototype.equals = function(v) {
          return this.compare(v) === 0;
        };
        NativeBigInt.prototype.eq = NativeBigInt.prototype.equals = SmallInteger.prototype.eq = SmallInteger.prototype.equals = BigInteger.prototype.eq = BigInteger.prototype.equals;
        BigInteger.prototype.notEquals = function(v) {
          return this.compare(v) !== 0;
        };
        NativeBigInt.prototype.neq = NativeBigInt.prototype.notEquals = SmallInteger.prototype.neq = SmallInteger.prototype.notEquals = BigInteger.prototype.neq = BigInteger.prototype.notEquals;
        BigInteger.prototype.greater = function(v) {
          return this.compare(v) > 0;
        };
        NativeBigInt.prototype.gt = NativeBigInt.prototype.greater = SmallInteger.prototype.gt = SmallInteger.prototype.greater = BigInteger.prototype.gt = BigInteger.prototype.greater;
        BigInteger.prototype.lesser = function(v) {
          return this.compare(v) < 0;
        };
        NativeBigInt.prototype.lt = NativeBigInt.prototype.lesser = SmallInteger.prototype.lt = SmallInteger.prototype.lesser = BigInteger.prototype.lt = BigInteger.prototype.lesser;
        BigInteger.prototype.greaterOrEquals = function(v) {
          return this.compare(v) >= 0;
        };
        NativeBigInt.prototype.geq = NativeBigInt.prototype.greaterOrEquals = SmallInteger.prototype.geq = SmallInteger.prototype.greaterOrEquals = BigInteger.prototype.geq = BigInteger.prototype.greaterOrEquals;
        BigInteger.prototype.lesserOrEquals = function(v) {
          return this.compare(v) <= 0;
        };
        NativeBigInt.prototype.leq = NativeBigInt.prototype.lesserOrEquals = SmallInteger.prototype.leq = SmallInteger.prototype.lesserOrEquals = BigInteger.prototype.leq = BigInteger.prototype.lesserOrEquals;
        BigInteger.prototype.isEven = function() {
          return (this.value[0] & 1) === 0;
        };
        SmallInteger.prototype.isEven = function() {
          return (this.value & 1) === 0;
        };
        NativeBigInt.prototype.isEven = function() {
          return (this.value & BigInt(1)) === BigInt(0);
        };
        BigInteger.prototype.isOdd = function() {
          return (this.value[0] & 1) === 1;
        };
        SmallInteger.prototype.isOdd = function() {
          return (this.value & 1) === 1;
        };
        NativeBigInt.prototype.isOdd = function() {
          return (this.value & BigInt(1)) === BigInt(1);
        };
        BigInteger.prototype.isPositive = function() {
          return !this.sign;
        };
        SmallInteger.prototype.isPositive = function() {
          return this.value > 0;
        };
        NativeBigInt.prototype.isPositive = SmallInteger.prototype.isPositive;
        BigInteger.prototype.isNegative = function() {
          return this.sign;
        };
        SmallInteger.prototype.isNegative = function() {
          return this.value < 0;
        };
        NativeBigInt.prototype.isNegative = SmallInteger.prototype.isNegative;
        BigInteger.prototype.isUnit = function() {
          return false;
        };
        SmallInteger.prototype.isUnit = function() {
          return Math.abs(this.value) === 1;
        };
        NativeBigInt.prototype.isUnit = function() {
          return this.abs().value === BigInt(1);
        };
        BigInteger.prototype.isZero = function() {
          return false;
        };
        SmallInteger.prototype.isZero = function() {
          return this.value === 0;
        };
        NativeBigInt.prototype.isZero = function() {
          return this.value === BigInt(0);
        };
        BigInteger.prototype.isDivisibleBy = function(v) {
          var n = parseValue(v);
          if (n.isZero())
            return false;
          if (n.isUnit())
            return true;
          if (n.compareAbs(2) === 0)
            return this.isEven();
          return this.mod(n).isZero();
        };
        NativeBigInt.prototype.isDivisibleBy = SmallInteger.prototype.isDivisibleBy = BigInteger.prototype.isDivisibleBy;
        function isBasicPrime(v) {
          var n = v.abs();
          if (n.isUnit())
            return false;
          if (n.equals(2) || n.equals(3) || n.equals(5))
            return true;
          if (n.isEven() || n.isDivisibleBy(3) || n.isDivisibleBy(5))
            return false;
          if (n.lesser(49))
            return true;
        }
        function millerRabinTest(n, a) {
          var nPrev = n.prev(), b = nPrev, r = 0, d, t, i2, x;
          while (b.isEven())
            b = b.divide(2), r++;
          next:
            for (i2 = 0; i2 < a.length; i2++) {
              if (n.lesser(a[i2]))
                continue;
              x = bigInt2(a[i2]).modPow(b, n);
              if (x.isUnit() || x.equals(nPrev))
                continue;
              for (d = r - 1; d != 0; d--) {
                x = x.square().mod(n);
                if (x.isUnit())
                  return false;
                if (x.equals(nPrev))
                  continue next;
              }
              return false;
            }
          return true;
        }
        BigInteger.prototype.isPrime = function(strict) {
          var isPrime = isBasicPrime(this);
          if (isPrime !== undefined2)
            return isPrime;
          var n = this.abs();
          var bits = n.bitLength();
          if (bits <= 64)
            return millerRabinTest(n, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]);
          var logN = Math.log(2) * bits.toJSNumber();
          var t = Math.ceil(strict === true ? 2 * Math.pow(logN, 2) : logN);
          for (var a = [], i2 = 0; i2 < t; i2++) {
            a.push(bigInt2(i2 + 2));
          }
          return millerRabinTest(n, a);
        };
        NativeBigInt.prototype.isPrime = SmallInteger.prototype.isPrime = BigInteger.prototype.isPrime;
        BigInteger.prototype.isProbablePrime = function(iterations, rng) {
          var isPrime = isBasicPrime(this);
          if (isPrime !== undefined2)
            return isPrime;
          var n = this.abs();
          var t = iterations === undefined2 ? 5 : iterations;
          for (var a = [], i2 = 0; i2 < t; i2++) {
            a.push(bigInt2.randBetween(2, n.minus(2), rng));
          }
          return millerRabinTest(n, a);
        };
        NativeBigInt.prototype.isProbablePrime = SmallInteger.prototype.isProbablePrime = BigInteger.prototype.isProbablePrime;
        BigInteger.prototype.modInv = function(n) {
          var t = bigInt2.zero, newT = bigInt2.one, r = parseValue(n), newR = this.abs(), q, lastT, lastR;
          while (!newR.isZero()) {
            q = r.divide(newR);
            lastT = t;
            lastR = r;
            t = newT;
            r = newR;
            newT = lastT.subtract(q.multiply(newT));
            newR = lastR.subtract(q.multiply(newR));
          }
          if (!r.isUnit())
            throw new Error(this.toString() + " and " + n.toString() + " are not co-prime");
          if (t.compare(0) === -1) {
            t = t.add(n);
          }
          if (this.isNegative()) {
            return t.negate();
          }
          return t;
        };
        NativeBigInt.prototype.modInv = SmallInteger.prototype.modInv = BigInteger.prototype.modInv;
        BigInteger.prototype.next = function() {
          var value = this.value;
          if (this.sign) {
            return subtractSmall(value, 1, this.sign);
          }
          return new BigInteger(addSmall(value, 1), this.sign);
        };
        SmallInteger.prototype.next = function() {
          var value = this.value;
          if (value + 1 < MAX_INT)
            return new SmallInteger(value + 1);
          return new BigInteger(MAX_INT_ARR, false);
        };
        NativeBigInt.prototype.next = function() {
          return new NativeBigInt(this.value + BigInt(1));
        };
        BigInteger.prototype.prev = function() {
          var value = this.value;
          if (this.sign) {
            return new BigInteger(addSmall(value, 1), true);
          }
          return subtractSmall(value, 1, this.sign);
        };
        SmallInteger.prototype.prev = function() {
          var value = this.value;
          if (value - 1 > -MAX_INT)
            return new SmallInteger(value - 1);
          return new BigInteger(MAX_INT_ARR, true);
        };
        NativeBigInt.prototype.prev = function() {
          return new NativeBigInt(this.value - BigInt(1));
        };
        var powersOfTwo = [1];
        while (2 * powersOfTwo[powersOfTwo.length - 1] <= BASE)
          powersOfTwo.push(2 * powersOfTwo[powersOfTwo.length - 1]);
        var powers2Length = powersOfTwo.length, highestPower2 = powersOfTwo[powers2Length - 1];
        function shift_isSmall(n) {
          return Math.abs(n) <= BASE;
        }
        BigInteger.prototype.shiftLeft = function(v) {
          var n = parseValue(v).toJSNumber();
          if (!shift_isSmall(n)) {
            throw new Error(String(n) + " is too large for shifting.");
          }
          if (n < 0)
            return this.shiftRight(-n);
          var result = this;
          if (result.isZero())
            return result;
          while (n >= powers2Length) {
            result = result.multiply(highestPower2);
            n -= powers2Length - 1;
          }
          return result.multiply(powersOfTwo[n]);
        };
        NativeBigInt.prototype.shiftLeft = SmallInteger.prototype.shiftLeft = BigInteger.prototype.shiftLeft;
        BigInteger.prototype.shiftRight = function(v) {
          var remQuo;
          var n = parseValue(v).toJSNumber();
          if (!shift_isSmall(n)) {
            throw new Error(String(n) + " is too large for shifting.");
          }
          if (n < 0)
            return this.shiftLeft(-n);
          var result = this;
          while (n >= powers2Length) {
            if (result.isZero() || result.isNegative() && result.isUnit())
              return result;
            remQuo = divModAny(result, highestPower2);
            result = remQuo[1].isNegative() ? remQuo[0].prev() : remQuo[0];
            n -= powers2Length - 1;
          }
          remQuo = divModAny(result, powersOfTwo[n]);
          return remQuo[1].isNegative() ? remQuo[0].prev() : remQuo[0];
        };
        NativeBigInt.prototype.shiftRight = SmallInteger.prototype.shiftRight = BigInteger.prototype.shiftRight;
        function bitwise(x, y, fn) {
          y = parseValue(y);
          var xSign = x.isNegative(), ySign = y.isNegative();
          var xRem = xSign ? x.not() : x, yRem = ySign ? y.not() : y;
          var xDigit = 0, yDigit = 0;
          var xDivMod = null, yDivMod = null;
          var result = [];
          while (!xRem.isZero() || !yRem.isZero()) {
            xDivMod = divModAny(xRem, highestPower2);
            xDigit = xDivMod[1].toJSNumber();
            if (xSign) {
              xDigit = highestPower2 - 1 - xDigit;
            }
            yDivMod = divModAny(yRem, highestPower2);
            yDigit = yDivMod[1].toJSNumber();
            if (ySign) {
              yDigit = highestPower2 - 1 - yDigit;
            }
            xRem = xDivMod[0];
            yRem = yDivMod[0];
            result.push(fn(xDigit, yDigit));
          }
          var sum2 = fn(xSign ? 1 : 0, ySign ? 1 : 0) !== 0 ? bigInt2(-1) : bigInt2(0);
          for (var i2 = result.length - 1; i2 >= 0; i2 -= 1) {
            sum2 = sum2.multiply(highestPower2).add(bigInt2(result[i2]));
          }
          return sum2;
        }
        BigInteger.prototype.not = function() {
          return this.negate().prev();
        };
        NativeBigInt.prototype.not = SmallInteger.prototype.not = BigInteger.prototype.not;
        BigInteger.prototype.and = function(n) {
          return bitwise(this, n, function(a, b) {
            return a & b;
          });
        };
        NativeBigInt.prototype.and = SmallInteger.prototype.and = BigInteger.prototype.and;
        BigInteger.prototype.or = function(n) {
          return bitwise(this, n, function(a, b) {
            return a | b;
          });
        };
        NativeBigInt.prototype.or = SmallInteger.prototype.or = BigInteger.prototype.or;
        BigInteger.prototype.xor = function(n) {
          return bitwise(this, n, function(a, b) {
            return a ^ b;
          });
        };
        NativeBigInt.prototype.xor = SmallInteger.prototype.xor = BigInteger.prototype.xor;
        var LOBMASK_I = 1 << 30, LOBMASK_BI = (BASE & -BASE) * (BASE & -BASE) | LOBMASK_I;
        function roughLOB(n) {
          var v = n.value, x = typeof v === "number" ? v | LOBMASK_I : typeof v === "bigint" ? v | BigInt(LOBMASK_I) : v[0] + v[1] * BASE | LOBMASK_BI;
          return x & -x;
        }
        function integerLogarithm(value, base) {
          if (base.compareTo(value) <= 0) {
            var tmp = integerLogarithm(value, base.square(base));
            var p = tmp.p;
            var e = tmp.e;
            var t = p.multiply(base);
            return t.compareTo(value) <= 0 ? { p: t, e: e * 2 + 1 } : { p, e: e * 2 };
          }
          return { p: bigInt2(1), e: 0 };
        }
        BigInteger.prototype.bitLength = function() {
          var n = this;
          if (n.compareTo(bigInt2(0)) < 0) {
            n = n.negate().subtract(bigInt2(1));
          }
          if (n.compareTo(bigInt2(0)) === 0) {
            return bigInt2(0);
          }
          return bigInt2(integerLogarithm(n, bigInt2(2)).e).add(bigInt2(1));
        };
        NativeBigInt.prototype.bitLength = SmallInteger.prototype.bitLength = BigInteger.prototype.bitLength;
        function max3(a, b) {
          a = parseValue(a);
          b = parseValue(b);
          return a.greater(b) ? a : b;
        }
        function min3(a, b) {
          a = parseValue(a);
          b = parseValue(b);
          return a.lesser(b) ? a : b;
        }
        function gcd(a, b) {
          a = parseValue(a).abs();
          b = parseValue(b).abs();
          if (a.equals(b))
            return a;
          if (a.isZero())
            return b;
          if (b.isZero())
            return a;
          var c = Integer[1], d, t;
          while (a.isEven() && b.isEven()) {
            d = min3(roughLOB(a), roughLOB(b));
            a = a.divide(d);
            b = b.divide(d);
            c = c.multiply(d);
          }
          while (a.isEven()) {
            a = a.divide(roughLOB(a));
          }
          do {
            while (b.isEven()) {
              b = b.divide(roughLOB(b));
            }
            if (a.greater(b)) {
              t = b;
              b = a;
              a = t;
            }
            b = b.subtract(a);
          } while (!b.isZero());
          return c.isUnit() ? a : a.multiply(c);
        }
        function lcm(a, b) {
          a = parseValue(a).abs();
          b = parseValue(b).abs();
          return a.divide(gcd(a, b)).multiply(b);
        }
        function randBetween(a, b, rng) {
          a = parseValue(a);
          b = parseValue(b);
          var usedRNG = rng || Math.random;
          var low = min3(a, b), high = max3(a, b);
          var range3 = high.subtract(low).add(1);
          if (range3.isSmall)
            return low.add(Math.floor(usedRNG() * range3));
          var digits = toBase2(range3, BASE).value;
          var result = [], restricted = true;
          for (var i2 = 0; i2 < digits.length; i2++) {
            var top2 = restricted ? digits[i2] + (i2 + 1 < digits.length ? digits[i2 + 1] / BASE : 0) : BASE;
            var digit = truncate(usedRNG() * top2);
            result.push(digit);
            if (digit < digits[i2])
              restricted = false;
          }
          return low.add(Integer.fromArray(result, BASE, false));
        }
        var parseBase = function(text4, base, alphabet, caseSensitive) {
          alphabet = alphabet || DEFAULT_ALPHABET;
          text4 = String(text4);
          if (!caseSensitive) {
            text4 = text4.toLowerCase();
            alphabet = alphabet.toLowerCase();
          }
          var length4 = text4.length;
          var i2;
          var absBase = Math.abs(base);
          var alphabetValues = {};
          for (i2 = 0; i2 < alphabet.length; i2++) {
            alphabetValues[alphabet[i2]] = i2;
          }
          for (i2 = 0; i2 < length4; i2++) {
            var c = text4[i2];
            if (c === "-")
              continue;
            if (c in alphabetValues) {
              if (alphabetValues[c] >= absBase) {
                if (c === "1" && absBase === 1)
                  continue;
                throw new Error(c + " is not a valid digit in base " + base + ".");
              }
            }
          }
          base = parseValue(base);
          var digits = [];
          var isNegative = text4[0] === "-";
          for (i2 = isNegative ? 1 : 0; i2 < text4.length; i2++) {
            var c = text4[i2];
            if (c in alphabetValues)
              digits.push(parseValue(alphabetValues[c]));
            else if (c === "<") {
              var start2 = i2;
              do {
                i2++;
              } while (text4[i2] !== ">" && i2 < text4.length);
              digits.push(parseValue(text4.slice(start2 + 1, i2)));
            } else
              throw new Error(c + " is not a valid character");
          }
          return parseBaseFromArray(digits, base, isNegative);
        };
        function parseBaseFromArray(digits, base, isNegative) {
          var val = Integer[0], pow4 = Integer[1], i2;
          for (i2 = digits.length - 1; i2 >= 0; i2--) {
            val = val.add(digits[i2].times(pow4));
            pow4 = pow4.times(base);
          }
          return isNegative ? val.negate() : val;
        }
        function stringify3(digit, alphabet) {
          alphabet = alphabet || DEFAULT_ALPHABET;
          if (digit < alphabet.length) {
            return alphabet[digit];
          }
          return "<" + digit + ">";
        }
        function toBase2(n, base) {
          base = bigInt2(base);
          if (base.isZero()) {
            if (n.isZero())
              return { value: [0], isNegative: false };
            throw new Error("Cannot convert nonzero numbers to base 0.");
          }
          if (base.equals(-1)) {
            if (n.isZero())
              return { value: [0], isNegative: false };
            if (n.isNegative())
              return {
                value: [].concat.apply(
                  [],
                  Array.apply(null, Array(-n.toJSNumber())).map(Array.prototype.valueOf, [1, 0])
                ),
                isNegative: false
              };
            var arr = Array.apply(null, Array(n.toJSNumber() - 1)).map(Array.prototype.valueOf, [0, 1]);
            arr.unshift([1]);
            return {
              value: [].concat.apply([], arr),
              isNegative: false
            };
          }
          var neg = false;
          if (n.isNegative() && base.isPositive()) {
            neg = true;
            n = n.abs();
          }
          if (base.isUnit()) {
            if (n.isZero())
              return { value: [0], isNegative: false };
            return {
              value: Array.apply(null, Array(n.toJSNumber())).map(Number.prototype.valueOf, 1),
              isNegative: neg
            };
          }
          var out = [];
          var left = n, divmod;
          while (left.isNegative() || left.compareAbs(base) >= 0) {
            divmod = left.divmod(base);
            left = divmod.quotient;
            var digit = divmod.remainder;
            if (digit.isNegative()) {
              digit = base.minus(digit).abs();
              left = left.next();
            }
            out.push(digit.toJSNumber());
          }
          out.push(left.toJSNumber());
          return { value: out.reverse(), isNegative: neg };
        }
        function toBaseString(n, base, alphabet) {
          var arr = toBase2(n, base);
          return (arr.isNegative ? "-" : "") + arr.value.map(function(x) {
            return stringify3(x, alphabet);
          }).join("");
        }
        BigInteger.prototype.toArray = function(radix) {
          return toBase2(this, radix);
        };
        SmallInteger.prototype.toArray = function(radix) {
          return toBase2(this, radix);
        };
        NativeBigInt.prototype.toArray = function(radix) {
          return toBase2(this, radix);
        };
        BigInteger.prototype.toString = function(radix, alphabet) {
          if (radix === undefined2)
            radix = 10;
          if (radix !== 10)
            return toBaseString(this, radix, alphabet);
          var v = this.value, l = v.length, str = String(v[--l]), zeros = "0000000", digit;
          while (--l >= 0) {
            digit = String(v[l]);
            str += zeros.slice(digit.length) + digit;
          }
          var sign2 = this.sign ? "-" : "";
          return sign2 + str;
        };
        SmallInteger.prototype.toString = function(radix, alphabet) {
          if (radix === undefined2)
            radix = 10;
          if (radix != 10)
            return toBaseString(this, radix, alphabet);
          return String(this.value);
        };
        NativeBigInt.prototype.toString = SmallInteger.prototype.toString;
        NativeBigInt.prototype.toJSON = BigInteger.prototype.toJSON = SmallInteger.prototype.toJSON = function() {
          return this.toString();
        };
        BigInteger.prototype.valueOf = function() {
          return parseInt(this.toString(), 10);
        };
        BigInteger.prototype.toJSNumber = BigInteger.prototype.valueOf;
        SmallInteger.prototype.valueOf = function() {
          return this.value;
        };
        SmallInteger.prototype.toJSNumber = SmallInteger.prototype.valueOf;
        NativeBigInt.prototype.valueOf = NativeBigInt.prototype.toJSNumber = function() {
          return parseInt(this.toString(), 10);
        };
        function parseStringValue(v) {
          if (isPrecise(+v)) {
            var x = +v;
            if (x === truncate(x))
              return supportsNativeBigInt ? new NativeBigInt(BigInt(x)) : new SmallInteger(x);
            throw new Error("Invalid integer: " + v);
          }
          var sign2 = v[0] === "-";
          if (sign2)
            v = v.slice(1);
          var split3 = v.split(/e/i);
          if (split3.length > 2)
            throw new Error("Invalid integer: " + split3.join("e"));
          if (split3.length === 2) {
            var exp2 = split3[1];
            if (exp2[0] === "+")
              exp2 = exp2.slice(1);
            exp2 = +exp2;
            if (exp2 !== truncate(exp2) || !isPrecise(exp2))
              throw new Error("Invalid integer: " + exp2 + " is not a valid exponent.");
            var text4 = split3[0];
            var decimalPlace = text4.indexOf(".");
            if (decimalPlace >= 0) {
              exp2 -= text4.length - decimalPlace - 1;
              text4 = text4.slice(0, decimalPlace) + text4.slice(decimalPlace + 1);
            }
            if (exp2 < 0)
              throw new Error("Cannot include negative exponent part for integers");
            text4 += new Array(exp2 + 1).join("0");
            v = text4;
          }
          var isValid2 = /^([0-9][0-9]*)$/.test(v);
          if (!isValid2)
            throw new Error("Invalid integer: " + v);
          if (supportsNativeBigInt) {
            return new NativeBigInt(BigInt(sign2 ? "-" + v : v));
          }
          var r = [], max4 = v.length, l = LOG_BASE, min4 = max4 - l;
          while (max4 > 0) {
            r.push(+v.slice(min4, max4));
            min4 -= l;
            if (min4 < 0)
              min4 = 0;
            max4 -= l;
          }
          trim2(r);
          return new BigInteger(r, sign2);
        }
        function parseNumberValue(v) {
          if (supportsNativeBigInt) {
            return new NativeBigInt(BigInt(v));
          }
          if (isPrecise(v)) {
            if (v !== truncate(v))
              throw new Error(v + " is not an integer.");
            return new SmallInteger(v);
          }
          return parseStringValue(v.toString());
        }
        function parseValue(v) {
          if (typeof v === "number") {
            return parseNumberValue(v);
          }
          if (typeof v === "string") {
            return parseStringValue(v);
          }
          if (typeof v === "bigint") {
            return new NativeBigInt(v);
          }
          return v;
        }
        for (var i = 0; i < 1e3; i++) {
          Integer[i] = parseValue(i);
          if (i > 0)
            Integer[-i] = parseValue(-i);
        }
        Integer.one = Integer[1];
        Integer.zero = Integer[0];
        Integer.minusOne = Integer[-1];
        Integer.max = max3;
        Integer.min = min3;
        Integer.gcd = gcd;
        Integer.lcm = lcm;
        Integer.isInstance = function(x) {
          return x instanceof BigInteger || x instanceof SmallInteger || x instanceof NativeBigInt;
        };
        Integer.randBetween = randBetween;
        Integer.fromArray = function(digits, base, isNegative) {
          return parseBaseFromArray(digits.map(parseValue), parseValue(base || 10), isNegative);
        };
        return Integer;
      }();
      if (typeof module !== "undefined" && module.hasOwnProperty("exports")) {
        module.exports = bigInt2;
      }
      if (typeof define === "function" && define.amd) {
        define(function() {
          return bigInt2;
        });
      }
    }
  });

  // output/Main/foreign.js
  var _geolocation = (right) => (left) => (cb) => () => {
    navigator.geolocation.getCurrentPosition((position) => {
      cb(
        right({
          latitude: position.coords.latitude,
          longitude: position.coords.longitude
        })
      )();
    });
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map13 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map13(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map13 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map13($$const(x))(f);
      };
    };
  };
  var voidRight = function(dictFunctor) {
    var map13 = map(dictFunctor);
    return function(x) {
      return map13($$const(x));
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map10 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map10($$const(identity2))(a))(b);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var when = function(dictApplicative) {
    var pure12 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure12(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure12 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply3(pure12(f))(a);
      };
    };
  };

  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i = 0, l = arr.length; i < l; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }
      return result;
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped1 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a) {
          return bindFlipped1(f)(g(a));
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count) {
    return function(value) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count; i++) {
        result[n++] = value;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head3) {
      return function(tail2) {
        return new Cons3(head3, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr3) {
      return function(xs) {
        return listToArray(foldr3(curryCons)(emptyList)(xs));
      };
    };
  }();
  var filter = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from3, to3) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to3 - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from3, mid);
      if (to3 - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to3);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to3) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to3) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label) {
    return function(rec) {
      return rec[label];
    };
  };
  var unsafeSet = function(label) {
    return function(value) {
      return function(rec) {
        var copy = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy[key] = rec[key];
          }
        }
        copy[label] = value;
        return copy;
      };
    };
  };
  var unsafeDelete = function(label) {
    return function(rec) {
      var copy = {};
      for (var key in rec) {
        if (key !== label && {}.hasOwnProperty.call(rec, key)) {
          copy[key] = rec[key];
        }
      }
      return copy;
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var pure8 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind5(f)(function(f$prime) {
          return bind5(a)(function(a$prime) {
            return pure8(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq3) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq3 : gt;
          };
        };
      };
    };
  };
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eqRowNil = {
    eqRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return true;
        };
      };
    }
  };
  var eqRecord = function(dict) {
    return dict.eqRecord;
  };
  var eqRec = function() {
    return function(dictEqRecord) {
      return {
        eq: eqRecord(dictEqRecord)($$Proxy.value)
      };
    };
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eqRowCons = function(dictEqRecord) {
    var eqRecord1 = eqRecord(dictEqRecord);
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(dictEq) {
          var eq3 = eq(dictEq);
          return {
            eqRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = eqRecord1($$Proxy.value)(ra)(rb);
                  var key = reflectSymbol2($$Proxy.value);
                  var get3 = unsafeGet(key);
                  return eq3(get3(ra))(get3(rb)) && tail2;
                };
              };
            }
          };
        };
      };
    };
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      function(c, i) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i + 1;
        var empty5 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty5;
      }
    ) + '"';
  };

  // output/Data.Show/index.js
  var showUnit = {
    show: function(v) {
      return "unit";
    }
  };
  var showString = {
    show: showStringImpl
  };
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record) {
            return "{" + (showRecordFields1($$Proxy.value)(record) + "}");
          }
        };
      };
    };
  };
  var showNumber = {
    show: showNumberImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show12 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields1($$Proxy.value)(record);
              var key = reflectSymbol2($$Proxy.value);
              var focus = unsafeGet(key)(record);
              return " " + (key + (": " + (show12(focus) + ("," + tail2))));
            };
          }
        };
      };
    };
  };
  var showRecordFieldsConsNil = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShow) {
      var show12 = show(dictShow);
      return {
        showRecordFields: function(v) {
          return function(record) {
            var key = reflectSymbol2($$Proxy.value);
            var focus = unsafeGet(key)(record);
            return " " + (key + (": " + (show12(focus) + " ")));
          };
        }
      };
    };
  };

  // output/Data.Maybe/index.js
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var showMaybe = function(dictShow) {
    var show4 = show(dictShow);
    return {
      show: function(v) {
        if (v instanceof Just) {
          return "(Just " + (show4(v.value0) + ")");
        }
        ;
        if (v instanceof Nothing) {
          return "Nothing";
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 223, column 1 - line 225, column 28): " + [v.constructor.name]);
      }
    };
  };
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var eqMaybe = function(dictEq) {
    var eq3 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq3(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var hush = /* @__PURE__ */ function() {
    return either($$const(Nothing.value))(Just.create);
  }();

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init4) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init4();
      state2 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var write = function(val) {
    return function(ref) {
      return function() {
        ref.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$new = _new;

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from3, to3) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to3 - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from3, mid);
      if (to3 - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to3);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to3) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to3) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var not = function(dict) {
    return dict.not;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();

  // output/Data.Bifunctor/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    var bimap1 = bimap(dictBifunctor);
    return function(f) {
      return bimap1(f)(identity3);
    };
  };
  var bifunctorEither = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return new Left(v(v2.value0));
          }
          ;
          if (v2 instanceof Right) {
            return new Right(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond3 = applySecond(dictApplicative.Apply0());
    var pure8 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($449) {
          return applySecond3(f($449));
        })(pure8(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append4 = append(dictMonoid.Semigroup0());
      var mempty4 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append4(f(x))(acc);
          };
        })(mempty4);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map10) {
        return function(pure8) {
          return function(f) {
            return function(array) {
              function go(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure8([]);
                  case 1:
                    return map10(array1)(f(array[bot]));
                  case 2:
                    return apply3(map10(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply3(apply3(map10(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply3(map10(concat2)(go(bot, pivot)))(go(pivot, top2));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Array/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var singleton2 = function(a) {
    return [a];
  };
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));

  // output/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };
  var toLower = function(s) {
    return s.toLowerCase();
  };
  var toUpper = function(s) {
    return s.toUpperCase();
  };

  // output/Data.String.Common/index.js
  var $$null = function(s) {
    return s === "";
  };

  // output/Data.HTTP.Method/index.js
  var OPTIONS = /* @__PURE__ */ function() {
    function OPTIONS2() {
    }
    ;
    OPTIONS2.value = new OPTIONS2();
    return OPTIONS2;
  }();
  var GET = /* @__PURE__ */ function() {
    function GET2() {
    }
    ;
    GET2.value = new GET2();
    return GET2;
  }();
  var HEAD = /* @__PURE__ */ function() {
    function HEAD2() {
    }
    ;
    HEAD2.value = new HEAD2();
    return HEAD2;
  }();
  var POST = /* @__PURE__ */ function() {
    function POST2() {
    }
    ;
    POST2.value = new POST2();
    return POST2;
  }();
  var PUT = /* @__PURE__ */ function() {
    function PUT2() {
    }
    ;
    PUT2.value = new PUT2();
    return PUT2;
  }();
  var DELETE = /* @__PURE__ */ function() {
    function DELETE2() {
    }
    ;
    DELETE2.value = new DELETE2();
    return DELETE2;
  }();
  var TRACE = /* @__PURE__ */ function() {
    function TRACE2() {
    }
    ;
    TRACE2.value = new TRACE2();
    return TRACE2;
  }();
  var CONNECT = /* @__PURE__ */ function() {
    function CONNECT2() {
    }
    ;
    CONNECT2.value = new CONNECT2();
    return CONNECT2;
  }();
  var PROPFIND = /* @__PURE__ */ function() {
    function PROPFIND2() {
    }
    ;
    PROPFIND2.value = new PROPFIND2();
    return PROPFIND2;
  }();
  var PROPPATCH = /* @__PURE__ */ function() {
    function PROPPATCH2() {
    }
    ;
    PROPPATCH2.value = new PROPPATCH2();
    return PROPPATCH2;
  }();
  var MKCOL = /* @__PURE__ */ function() {
    function MKCOL2() {
    }
    ;
    MKCOL2.value = new MKCOL2();
    return MKCOL2;
  }();
  var COPY = /* @__PURE__ */ function() {
    function COPY2() {
    }
    ;
    COPY2.value = new COPY2();
    return COPY2;
  }();
  var MOVE = /* @__PURE__ */ function() {
    function MOVE2() {
    }
    ;
    MOVE2.value = new MOVE2();
    return MOVE2;
  }();
  var LOCK = /* @__PURE__ */ function() {
    function LOCK2() {
    }
    ;
    LOCK2.value = new LOCK2();
    return LOCK2;
  }();
  var UNLOCK = /* @__PURE__ */ function() {
    function UNLOCK2() {
    }
    ;
    UNLOCK2.value = new UNLOCK2();
    return UNLOCK2;
  }();
  var PATCH = /* @__PURE__ */ function() {
    function PATCH2() {
    }
    ;
    PATCH2.value = new PATCH2();
    return PATCH2;
  }();
  var showMethod = {
    show: function(v) {
      if (v instanceof OPTIONS) {
        return "OPTIONS";
      }
      ;
      if (v instanceof GET) {
        return "GET";
      }
      ;
      if (v instanceof HEAD) {
        return "HEAD";
      }
      ;
      if (v instanceof POST) {
        return "POST";
      }
      ;
      if (v instanceof PUT) {
        return "PUT";
      }
      ;
      if (v instanceof DELETE) {
        return "DELETE";
      }
      ;
      if (v instanceof TRACE) {
        return "TRACE";
      }
      ;
      if (v instanceof CONNECT) {
        return "CONNECT";
      }
      ;
      if (v instanceof PROPFIND) {
        return "PROPFIND";
      }
      ;
      if (v instanceof PROPPATCH) {
        return "PROPPATCH";
      }
      ;
      if (v instanceof MKCOL) {
        return "MKCOL";
      }
      ;
      if (v instanceof COPY) {
        return "COPY";
      }
      ;
      if (v instanceof MOVE) {
        return "MOVE";
      }
      ;
      if (v instanceof LOCK) {
        return "LOCK";
      }
      ;
      if (v instanceof UNLOCK) {
        return "UNLOCK";
      }
      ;
      if (v instanceof PATCH) {
        return "PATCH";
      }
      ;
      throw new Error("Failed pattern match at Data.HTTP.Method (line 43, column 1 - line 59, column 23): " + [v.constructor.name]);
    }
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error3) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error3) {
        setTimeout(function() {
          throw error3;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error3) {
        return left(error3);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error3) {
        k(left(error3))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size3 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size3 !== 0) {
          size3--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i, tmp;
          if (size3 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size3) % limit] = cb;
          size3++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error3) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status2 = SUSPENDED;
      var step2 = aff;
      var fail2 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run4(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status2) {
            case STEP_BIND:
              status2 = CONTINUE;
              try {
                step2 = bhead(step2);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status2 = RETURN;
                fail2 = util.left(e);
                step2 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step2)) {
                status2 = RETURN;
                fail2 = step2;
                step2 = null;
              } else if (bhead === null) {
                status2 = RETURN;
              } else {
                status2 = STEP_BIND;
                step2 = util.fromRight(step2);
              }
              break;
            case CONTINUE:
              switch (step2.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step2._2;
                  status2 = CONTINUE;
                  step2 = step2._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status2 = RETURN;
                    step2 = util.right(step2._1);
                  } else {
                    status2 = STEP_BIND;
                    step2 = step2._1;
                  }
                  break;
                case SYNC:
                  status2 = STEP_RESULT;
                  step2 = runSync(util.left, util.right, step2._1);
                  break;
                case ASYNC:
                  status2 = PENDING;
                  step2 = runAsync(util.left, step2._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status2 = STEP_RESULT;
                        step2 = result2;
                        run4(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status2 = RETURN;
                  fail2 = util.left(step2._1);
                  step2 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status2 = CONTINUE;
                  step2 = step2._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status2 = CONTINUE;
                  step2 = step2._1;
                  break;
                case FORK:
                  status2 = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step2._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step2._1) {
                    tmp.run();
                  }
                  step2 = util.right(tmp);
                  break;
                case SEQ:
                  status2 = CONTINUE;
                  step2 = sequential2(util, supervisor, step2._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status2 = COMPLETED;
                step2 = interrupt || fail2 || step2;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status2 = RETURN;
                    } else if (fail2) {
                      status2 = CONTINUE;
                      step2 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status2 = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status2 = STEP_BIND;
                      step2 = util.fromRight(step2);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step2);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status2 = CONTINUE;
                        step2 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail2), attempts, interrupt);
                    status2 = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step2 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step2 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step2 = attempt._1.completed(util.fromRight(step2))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail2), attempts, interrupt);
                    status2 = CONTINUE;
                    step2 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status2 = RETURN;
                    step2 = attempt._1;
                    fail2 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step2));
                }
              }
              joins = null;
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step2) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step2);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status2 = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join3) {
        return function() {
          if (status2 === COMPLETED) {
            rethrow = rethrow && join3.rethrow;
            join3.handler(step2)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join3;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error3, cb) {
        return function() {
          if (status2 === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status2) {
            case SUSPENDED:
              interrupt = util.left(error3);
              status2 = COMPLETED;
              step2 = interrupt;
              run4(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                if (status2 === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step2(error3)), attempts, interrupt);
                }
                status2 = RETURN;
                step2 = null;
                fail2 = null;
                run4(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                status2 = RETURN;
                step2 = null;
                fail2 = null;
              }
          }
          return canceler;
        };
      }
      function join2(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status2 === SUSPENDED) {
            run4(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join: join2,
        onComplete,
        isSuspended: function() {
          return status2 === SUSPENDED;
        },
        run: function() {
          if (status2 === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run4(runTick);
              });
            } else {
              run4(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill(error3, par2, cb2) {
        var step2 = par2;
        var head3 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step2.tag) {
              case FORKED:
                if (step2._3 === EMPTY) {
                  tmp = fibers[step2._1];
                  kills2[count++] = tmp.kill(error3, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head3 === null) {
                  break loop;
                }
                step2 = head3._2;
                if (tail2 === null) {
                  head3 = null;
                } else {
                  head3 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step2 = step2._2;
                break;
              case APPLY:
              case ALT:
                if (head3) {
                  tail2 = new Aff2(CONS, head3, tail2);
                }
                head3 = step2;
                step2 = step2._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join2(result, head3, tail2) {
        var fail2, step2, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step2 = null;
        } else {
          step2 = result;
          fail2 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head3 === null) {
              cb(fail2 || step2)();
              return;
            }
            if (head3._3 !== EMPTY) {
              return;
            }
            switch (head3.tag) {
              case MAP:
                if (fail2 === null) {
                  head3._3 = util.right(head3._1(util.fromRight(step2)));
                  step2 = head3._3;
                } else {
                  head3._3 = fail2;
                }
                break;
              case APPLY:
                lhs = head3._1._3;
                rhs = head3._2._3;
                if (fail2) {
                  head3._3 = fail2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, fail2 === lhs ? head3._2 : head3._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join2(fail2, null, null);
                      } else {
                        join2(fail2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step2 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head3._3 = step2;
                }
                break;
              case ALT:
                lhs = head3._1._3;
                rhs = head3._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail2 = step2 === lhs ? rhs : lhs;
                  step2 = null;
                  head3._3 = fail2;
                } else {
                  head3._3 = step2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, step2 === lhs ? head3._2 : head3._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join2(step2, null, null);
                      } else {
                        join2(step2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head3 = null;
            } else {
              head3 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join2(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run4() {
        var status2 = CONTINUE;
        var step2 = par;
        var head3 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status2) {
              case CONTINUE:
                switch (step2.tag) {
                  case MAP:
                    if (head3) {
                      tail2 = new Aff2(CONS, head3, tail2);
                    }
                    head3 = new Aff2(MAP, step2._1, EMPTY, EMPTY);
                    step2 = step2._2;
                    break;
                  case APPLY:
                    if (head3) {
                      tail2 = new Aff2(CONS, head3, tail2);
                    }
                    head3 = new Aff2(APPLY, EMPTY, step2._2, EMPTY);
                    step2 = step2._1;
                    break;
                  case ALT:
                    if (head3) {
                      tail2 = new Aff2(CONS, head3, tail2);
                    }
                    head3 = new Aff2(ALT, EMPTY, step2._2, EMPTY);
                    step2 = step2._1;
                    break;
                  default:
                    fid = fiberId++;
                    status2 = RETURN;
                    tmp = step2;
                    step2 = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step2)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head3 === null) {
                  break loop;
                }
                if (head3._1 === EMPTY) {
                  head3._1 = step2;
                  status2 = CONTINUE;
                  step2 = head3._2;
                  head3._2 = EMPTY;
                } else {
                  head3._2 = step2;
                  step2 = head3;
                  if (tail2 === null) {
                    head3 = null;
                  } else {
                    head3 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step2;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error3, cb2) {
        interrupt = util.left(error3);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill(error3, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run4();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value) {
          return Aff.Pure(f(value));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer = setDelay(ms, cb(right()));
          return function() {
            return Aff.Sync(function() {
              return right(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }
  function catchException(c) {
    return function(t) {
      return function() {
        try {
          return t();
        } catch (e) {
          if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
            return c(e)();
          } else {
            return c(new Error(e.toString()))();
          }
        }
      };
    };
  }

  // output/Effect.Exception/index.js
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var map2 = /* @__PURE__ */ map(functorEffect);
  var $$try = function(action) {
    return catchException(function($3) {
      return pure2(Left.create($3));
    })(map2(Right.create)(action));
  };
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try2 = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map10 = map(Monad0.Bind1().Apply0().Functor0());
    var pure8 = pure(Monad0.Applicative0());
    return function(a) {
      return catchError1(map10(Right.create)(a))(function($52) {
        return pure8(Left.create($52));
      });
    };
  };

  // output/Effect.Class/index.js
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map3 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var withExceptT = function(dictFunctor) {
    var map13 = map(dictFunctor);
    return function(f) {
      return function(v) {
        var mapLeft = function(v1) {
          return function(v2) {
            if (v2 instanceof Right) {
              return new Right(v2.value0);
            }
            ;
            if (v2 instanceof Left) {
              return new Left(v1(v2.value0));
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 42, column 3 - line 42, column 32): " + [v1.constructor.name, v2.constructor.name]);
          };
        };
        return map13(mapLeft(f))(v);
      };
    };
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map13 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map13(map3(f)));
      }
    };
  };
  var except = function(dictApplicative) {
    var $185 = pure(dictApplicative);
    return function($186) {
      return ExceptT($185($186));
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var pure8 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind5(v)(either(function($187) {
            return pure8(Left.create($187));
          })(function(a) {
            var v1 = k(a);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append4 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind5 = bind(Bind1);
      var pure8 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind5(v)(function(rm) {
              if (rm instanceof Right) {
                return pure8(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind5(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure8(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure8(new Left(append4(rm.value0)(rn.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [rn.constructor.name]);
                });
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [rm.constructor.name]);
            });
          };
        },
        Functor0: function() {
          return functorExceptT1;
        }
      };
    };
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Control.Parallel/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential2 = sequential(dictParallel);
    var traverse_3 = traverse_(dictParallel.Applicative1());
    var parallel2 = parallel(dictParallel);
    return function(dictFoldable) {
      var traverse_1 = traverse_3(dictFoldable);
      return function(f) {
        var $48 = traverse_1(function($50) {
          return parallel2(f($50));
        });
        return function($49) {
          return sequential2($48($49));
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictFoldable) {
      return parTraverse_1(dictFoldable)(identity4);
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy2 = function(name2, moduleName, init4) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init4();
      state2 = 2;
      return val;
    };
  };
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindAff);
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try3 = /* @__PURE__ */ $$try2(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped2(function($80) {
        return liftEffect2(k($80));
      })($$try3(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void2(runAff(k)(aff));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Monad0: function() {
      return monadAff;
    },
    Applicative1: function() {
      return $lazy_applicativeParAff(0);
    }
  };
  var $lazy_applicativeParAff = /* @__PURE__ */ $runtime_lazy2("applicativeParAff", "Effect.Aff", function() {
    return {
      pure: function() {
        var $82 = parallel(parallelAff);
        return function($83) {
          return $82(pure22($83));
        };
      }(),
      Apply0: function() {
        return applyParAff;
      }
    };
  });
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(foldableArray);
  var semigroupCanceler = {
    append: function(v) {
      return function(v1) {
        return function(err) {
          return parSequence_2([v(err), v1(err)]);
        };
      };
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));
  var monoidCanceler = {
    mempty: nonCanceler,
    Semigroup0: function() {
      return semigroupCanceler;
    }
  };

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };
  var liftAff = function(dict) {
    return dict.liftAff;
  };

  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Control.Promise/foreign.js
  function thenImpl(promise2) {
    return function(errCB) {
      return function(succCB) {
        return function() {
          promise2.then(succCB, errCB);
        };
      };
    };
  }

  // output/Control.Monad.Except/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var withExcept = /* @__PURE__ */ withExceptT(functorIdentity);
  var runExcept = function($3) {
    return unwrap2(runExceptT($3));
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton3 = function(dictPlus) {
    var empty5 = empty(dictPlus);
    return function(a) {
      return new NonEmpty(a, empty5);
    };
  };
  var functorNonEmpty = function(dictFunctor) {
    var map23 = map(dictFunctor);
    return {
      map: function(f) {
        return function(m) {
          return new NonEmpty(f(m.value0), map23(f)(m.value1));
        };
      }
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_chunksAcc) {
      return function($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v1) {
            if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }
            ;
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v1, acc) {
                if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }
                ;
                $tco_done1 = true;
                return acc;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var map4 = /* @__PURE__ */ map(functorList);
  var functorNonEmptyList = /* @__PURE__ */ functorNonEmpty(functorList);
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev = function() {
          var go = function($copy_acc) {
            return function($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_acc = new Cons(v.value0, acc);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go(Nil.value);
        }();
        var $281 = foldl(foldableList)(flip(f))(b);
        return function($282) {
          return $281(rev($282));
        };
      };
    },
    foldl: function(f) {
      var go = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty4 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $283 = append22(acc);
          return function($284) {
            return $283(f($284));
          };
        })(mempty4);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
      };
    }
  };
  var applyList = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(map4(v.value0)(v1))(apply(applyList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorList;
    }
  };
  var apply2 = /* @__PURE__ */ apply(applyList);
  var applyNonEmptyList = {
    apply: function(v) {
      return function(v1) {
        return new NonEmpty(v.value0(v1.value0), append1(apply2(v.value1)(new Cons(v1.value0, Nil.value)))(apply2(new Cons(v.value0, v.value1))(v1.value1)));
      };
    },
    Functor0: function() {
      return functorNonEmptyList;
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();
  var applicativeNonEmptyList = {
    pure: /* @__PURE__ */ function() {
      var $312 = singleton3(plusList);
      return function($313) {
        return NonEmptyList($312($313));
      };
    }(),
    Apply0: function() {
      return applyNonEmptyList;
    }
  };

  // output/Foreign/foreign.js
  function typeOf(value) {
    return typeof value;
  }
  function tagOf(value) {
    return Object.prototype.toString.call(value).slice(8, -1);
  }
  function isNull(value) {
    return value === null;
  }
  function isUndefined(value) {
    return value === void 0;
  }
  var isArray = Array.isArray || function(value) {
    return Object.prototype.toString.call(value) === "[object Array]";
  };

  // output/Data.List.NonEmpty/index.js
  var singleton4 = /* @__PURE__ */ function() {
    var $199 = singleton3(plusList);
    return function($200) {
      return NonEmptyList($199($200));
    };
  }();

  // output/Data.String.CodeUnits/foreign.js
  var singleton5 = function(c) {
    return c;
  };
  var length3 = function(s) {
    return s.length;
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Foreign/index.js
  var ForeignError = /* @__PURE__ */ function() {
    function ForeignError2(value0) {
      this.value0 = value0;
    }
    ;
    ForeignError2.create = function(value0) {
      return new ForeignError2(value0);
    };
    return ForeignError2;
  }();
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return function(value1) {
        return new TypeMismatch3(value0, value1);
      };
    };
    return TypeMismatch3;
  }();
  var ErrorAtProperty = /* @__PURE__ */ function() {
    function ErrorAtProperty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtProperty2.create = function(value0) {
      return function(value1) {
        return new ErrorAtProperty2(value0, value1);
      };
    };
    return ErrorAtProperty2;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton4($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure12 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value) {
        if (tagOf(value) === tag) {
          return pure12(unsafeFromForeign(value));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value.constructor.name]);
      };
    };
  };
  var readNumber = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("Number");
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Control.Promise/index.js
  var voidRight2 = /* @__PURE__ */ voidRight(functorEffect);
  var mempty2 = /* @__PURE__ */ mempty(monoidCanceler);
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var alt2 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var map5 = /* @__PURE__ */ map(/* @__PURE__ */ functorExceptT(functorIdentity));
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var bind2 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var toAff$prime = function(customCoerce) {
    return function(p) {
      return makeAff(function(cb) {
        return voidRight2(mempty2)(thenImpl(p)(function($14) {
          return cb(Left.create(customCoerce($14)))();
        })(function($15) {
          return cb(Right.create($15))();
        }));
      });
    };
  };
  var coerce3 = function(fn) {
    return either(function(v) {
      return error("Promise failed, couldn't extract JS Error or String");
    })(identity5)(runExcept(alt2(unsafeReadTagged2("Error")(fn))(map5(error)(readString2(fn)))));
  };
  var toAff = /* @__PURE__ */ toAff$prime(coerce3);
  var toAffE = function(f) {
    return bind2(liftEffect3(f))(toAff);
  };

  // output/Fetch.Core/foreign.js
  function _fetch(a, b) {
    return fetch(a, b);
  }

  // output/Effect.Uncurried/foreign.js
  var runEffectFn1 = function runEffectFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runEffectFn2 = function runEffectFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };
  var runEffectFn4 = function runEffectFn42(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function() {
              return fn(a, b, c, d);
            };
          };
        };
      };
    };
  };

  // output/Fetch.Core/index.js
  var fetch2 = function(req) {
    return function() {
      return _fetch(req, {});
    };
  };

  // output/Fetch.Core.Headers/foreign.js
  function unsafeFromRecord(r) {
    return new Headers(r);
  }

  // output/Fetch.Core.Headers/index.js
  var fromRecord = function() {
    return unsafeFromRecord;
  };

  // output/Fetch.Core.Request/foreign.js
  function _unsafeNew(url2, options) {
    try {
      return new Request(url2, options);
    } catch (e) {
      console.error(e);
      throw e;
    }
  }

  // output/Record/index.js
  var insert = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(a) {
            return function(r) {
              return unsafeSet(reflectSymbol2(l))(a)(r);
            };
          };
        };
      };
    };
  };
  var get2 = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol2(l))(r);
        };
      };
    };
  };
  var $$delete = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(r) {
            return unsafeDelete(reflectSymbol2(l))(r);
          };
        };
      };
    };
  };

  // output/Fetch.Core.RequestBody/foreign.js
  function fromString(a) {
    return a;
  }

  // output/Fetch.Internal.RequestBody/index.js
  var toRequestBodyString = {
    toRequestBody: fromString
  };
  var toRequestBody = function(dict) {
    return dict.toRequestBody;
  };

  // output/Fetch.Internal.Request/index.js
  var fromRecord2 = /* @__PURE__ */ fromRecord();
  var show2 = /* @__PURE__ */ show(showMethod);
  var toCoreRequestOptionsHelpe = {
    convertHelper: function(v) {
      return function(v1) {
        return {};
      };
    }
  };
  var toCoreRequestOptionsConve6 = function(dictToRequestBody) {
    var toRequestBody2 = toRequestBody(dictToRequestBody);
    return {
      convertImpl: function(v) {
        return toRequestBody2;
      }
    };
  };
  var toCoreRequestOptionsConve7 = function() {
    return {
      convertImpl: function(v) {
        return fromRecord2;
      }
    };
  };
  var toCoreRequestOptionsConve8 = {
    convertImpl: function(v) {
      return show2;
    }
  };
  var $$new2 = function() {
    return function(url2) {
      return function(options) {
        return function() {
          return _unsafeNew(url2, options);
        };
      };
    };
  };
  var convertImpl = function(dict) {
    return dict.convertImpl;
  };
  var convertHelper = function(dict) {
    return dict.convertHelper;
  };
  var toCoreRequestOptionsHelpe1 = function(dictToCoreRequestOptionsConverter) {
    var convertImpl1 = convertImpl(dictToCoreRequestOptionsConverter);
    return function() {
      return function() {
        return function() {
          return function(dictIsSymbol) {
            var $$delete4 = $$delete(dictIsSymbol)()();
            var get3 = get2(dictIsSymbol)();
            var insert7 = insert(dictIsSymbol)()();
            return function(dictToCoreRequestOptionsHelper) {
              var convertHelper1 = convertHelper(dictToCoreRequestOptionsHelper);
              return function() {
                return function() {
                  return {
                    convertHelper: function(v) {
                      return function(r) {
                        var tail2 = convertHelper1($$Proxy.value)($$delete4($$Proxy.value)(r));
                        var head3 = convertImpl1($$Proxy.value)(get3($$Proxy.value)(r));
                        return insert7($$Proxy.value)(head3)(tail2);
                      };
                    }
                  };
                };
              };
            };
          };
        };
      };
    };
  };
  var toCoreRequestOptionsRowRo = function() {
    return function() {
      return function(dictToCoreRequestOptionsHelper) {
        return {
          convert: convertHelper(dictToCoreRequestOptionsHelper)($$Proxy.value)
        };
      };
    };
  };
  var convert = function(dict) {
    return dict.convert;
  };

  // output/Fetch.Core.Response/foreign.js
  function headers(resp) {
    return resp.headers;
  }
  function ok(resp) {
    return resp.ok;
  }
  function redirected(resp) {
    return resp.redirected;
  }
  function status(resp) {
    return resp.status;
  }
  function statusText(resp) {
    return resp.statusText;
  }
  function url(resp) {
    return resp.url;
  }
  function body(resp) {
    return function() {
      return resp.body;
    };
  }
  function arrayBuffer(resp) {
    return function() {
      return resp.arrayBuffer();
    };
  }
  function blob(resp) {
    return function() {
      return resp.blob();
    };
  }
  function text(resp) {
    return function() {
      return resp.text();
    };
  }
  function json(resp) {
    return function() {
      return resp.json();
    };
  }

  // output/Fetch.Internal.Response/index.js
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorEffect);
  var promiseToPromise = unsafeCoerce2;
  var text2 = function(response) {
    return toAffE(mapFlipped2(text(response))(promiseToPromise));
  };
  var json2 = function(response) {
    return toAffE(mapFlipped2(json(response))(promiseToPromise));
  };
  var blob2 = function(response) {
    return toAffE(mapFlipped2(blob(response))(promiseToPromise));
  };
  var arrayBuffer2 = function(response) {
    return toAffE(mapFlipped2(arrayBuffer(response))(promiseToPromise));
  };
  var convert2 = function(response) {
    return {
      headers: headers(response),
      ok: ok(response),
      redirected: redirected(response),
      status: status(response),
      statusText: statusText(response),
      url: url(response),
      text: text2(response),
      json: json2(response),
      body: body(response),
      arrayBuffer: arrayBuffer2(response),
      blob: blob2(response)
    };
  };

  // output/Fetch/index.js
  var bind3 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var $$new3 = /* @__PURE__ */ $$new2();
  var map6 = /* @__PURE__ */ map(functorEffect);
  var pure3 = /* @__PURE__ */ pure(applicativeAff);
  var fetch3 = function() {
    return function() {
      return function(dictToCoreRequestOptions) {
        var convert3 = convert(dictToCoreRequestOptions);
        return function(url2) {
          return function(r) {
            return bind3(liftEffect4($$new3(url2)(convert3(r))))(function(request) {
              return bind3(toAffE(map6(promiseToPromise)(fetch2(request))))(function(cResponse) {
                return pure3(convert2(cResponse));
              });
            });
          };
        };
      };
    };
  };

  // output/Flame.Application.Internal.Dom/foreign.js
  function querySelector_(selector) {
    return document.querySelector(selector);
  }
  function createWindowListener_(eventName, updater) {
    window.addEventListener(eventName, function(event) {
      updater(event)();
    });
  }
  function createDocumentListener_(eventName, updater) {
    document.addEventListener(eventName, function(event) {
      updater(event)();
    });
  }
  function createCustomListener_(eventName, updater) {
    document.addEventListener(eventName, function(event) {
      updater(event.detail)();
    });
  }

  // output/Data.Nullable/foreign.js
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Flame.Application.Internal.Dom/index.js
  var querySelector = function(selector) {
    return function __do() {
      var selected = querySelector_(selector);
      return toMaybe(selected);
    };
  };
  var createWindowListener = /* @__PURE__ */ runEffectFn2(createWindowListener_);
  var createDocumentListener = /* @__PURE__ */ runEffectFn2(createDocumentListener_);
  var createCustomListener = /* @__PURE__ */ runEffectFn2(createCustomListener_);

  // output/Data.String.Regex/foreign.js
  var regexImpl = function(left) {
    return function(right) {
      return function(s1) {
        return function(s2) {
          try {
            return right(new RegExp(s1, s2));
          } catch (e) {
            return left(e.message);
          }
        };
      };
    };
  };
  var _replaceBy = function(just) {
    return function(nothing) {
      return function(r) {
        return function(f) {
          return function(s) {
            return s.replace(r, function(match) {
              var groups = [];
              var group3, i = 1;
              while (typeof (group3 = arguments[i++]) !== "number") {
                groups.push(group3 == null ? nothing : just(group3));
              }
              return f(match)(groups);
            });
          };
        };
      };
    };
  };

  // output/Data.String.Regex.Flags/index.js
  var global = {
    global: true,
    ignoreCase: false,
    multiline: false,
    dotAll: false,
    sticky: false,
    unicode: false
  };

  // output/Data.String.Regex/index.js
  var replace$prime = /* @__PURE__ */ function() {
    return _replaceBy(Just.create)(Nothing.value);
  }();
  var renderFlags = function(v) {
    return function() {
      if (v.global) {
        return "g";
      }
      ;
      return "";
    }() + (function() {
      if (v.ignoreCase) {
        return "i";
      }
      ;
      return "";
    }() + (function() {
      if (v.multiline) {
        return "m";
      }
      ;
      return "";
    }() + (function() {
      if (v.dotAll) {
        return "s";
      }
      ;
      return "";
    }() + (function() {
      if (v.sticky) {
        return "y";
      }
      ;
      return "";
    }() + function() {
      if (v.unicode) {
        return "u";
      }
      ;
      return "";
    }()))));
  };
  var regex = function(s) {
    return function(f) {
      return regexImpl(Left.create)(Right.create)(s)(renderFlags(f));
    };
  };

  // output/Flame.Html.Attribute.Internal/foreign.js
  var classData = 2;
  var propertyData = 3;
  function createProperty_(name2) {
    return function(value) {
      return [propertyData, name2, value];
    };
  }
  function createClass(array) {
    return [classData, array];
  }

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom2 = bottom(dictBoundedEnum.Bounded0());
    return function(low) {
      return function(high) {
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom2);
            if ($140) {
              return low;
            }
            ;
            return high;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.String.CodePoints/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons2 = function(s) {
    var v = length3(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $42 = isLead(cu0) && isTrail(cu1);
    if ($42) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $74 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($75) {
      return singleton5($74($75));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton6 = /* @__PURE__ */ _singleton(singletonFallback);

  // output/Foreign.Object/foreign.js
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Flame.Html.Attribute.Internal/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var crashWith3 = /* @__PURE__ */ crashWith();
  var show22 = /* @__PURE__ */ show(showString);
  var map7 = /* @__PURE__ */ map(functorArray);
  var to = function(dict) {
    return dict.to;
  };
  var stringClassList = {
    to: /* @__PURE__ */ function() {
      var $41 = filter(function() {
        var $44 = not(heytingAlgebraBoolean);
        return function($45) {
          return $44($$null($45));
        };
      }());
      var $42 = split(" ");
      return function($43) {
        return $41($42($43));
      };
    }()
  };
  var createProperty = function(name1) {
    return function(value1) {
      return createProperty_(name1)(value1);
    };
  };
  var id = /* @__PURE__ */ createProperty("id");
  var src = /* @__PURE__ */ createProperty("src");
  var caseify = function(name$prime) {
    if (name$prime === toUpper(name$prime)) {
      return toLower(name$prime);
    }
    ;
    if (otherwise) {
      var v = fromJust2(uncons2(name$prime));
      var replacer2 = function($128) {
        return $$const(function(v1) {
          return "-" + v1;
        }(toLower($128)));
      };
      var regex2 = function() {
        var v1 = regex("[A-Z]")(global);
        if (v1 instanceof Right) {
          return v1.value0;
        }
        ;
        if (v1 instanceof Left) {
          return crashWith3(show22(v1.value0));
        }
        ;
        throw new Error("Failed pattern match at Flame.Html.Attribute.Internal (line 98, column 38 - line 100, column 56): " + [v1.constructor.name]);
      }();
      var hyphenated = replace$prime(regex2)(replacer2)(v.tail);
      return toLower(singleton6(v.head)) + hyphenated;
    }
    ;
    throw new Error("Failed pattern match at Flame.Html.Attribute.Internal (line 92, column 1 - line 92, column 28): " + [name$prime.constructor.name]);
  };
  var class$prime = function(dictToClassList) {
    var $129 = map7(caseify);
    var $130 = to(dictToClassList);
    return function($131) {
      return createClass($129($130($131)));
    };
  };
  var alt3 = /* @__PURE__ */ createProperty("alt");

  // output/Flame.Html.Element/foreign.js
  var textNode = 1;
  var elementNode = 2;
  var styleData = 1;
  var classData2 = 2;
  var propertyData2 = 3;
  var attributeData = 4;
  var keyData = 7;
  function createElementNode(tag) {
    return function(nodeData) {
      return function(potentialChildren) {
        let children = potentialChildren, text4 = void 0;
        if (potentialChildren.length === 1 && potentialChildren[0].nodeType == textNode) {
          children = void 0;
          text4 = potentialChildren[0].text;
        }
        return {
          nodeType: elementNode,
          node: void 0,
          tag,
          nodeData: fromNodeData(nodeData),
          children,
          text: text4
        };
      };
    };
  }
  function createDatalessElementNode(tag) {
    return function(potentialChildren) {
      let children = potentialChildren, text4 = void 0;
      if (potentialChildren.length === 1 && potentialChildren[0].nodeType == textNode) {
        children = void 0;
        text4 = potentialChildren[0].text;
      }
      return {
        nodeType: elementNode,
        node: void 0,
        tag,
        nodeData: {},
        children,
        text: text4
      };
    };
  }
  function createSingleElementNode(tag) {
    return function(nodeData) {
      return {
        nodeType: elementNode,
        node: void 0,
        tag,
        nodeData: fromNodeData(nodeData)
      };
    };
  }
  function text3(value) {
    return {
      nodeType: textNode,
      node: void 0,
      text: value
    };
  }
  function fromNodeData(allData) {
    let nodeData = {};
    if (allData !== void 0)
      for (let data of allData) {
        let dataOne = data[1];
        switch (data[0]) {
          case styleData:
            if (nodeData.styles === void 0)
              nodeData.styles = {};
            for (let key in dataOne)
              nodeData.styles[key] = dataOne[key];
            break;
          case classData2:
            if (nodeData.classes === void 0)
              nodeData.classes = [];
            nodeData.classes = nodeData.classes.concat(dataOne);
            break;
          case propertyData2:
            if (nodeData.properties === void 0)
              nodeData.properties = {};
            nodeData.properties[dataOne] = data[2];
            break;
          case attributeData:
            if (nodeData.attributes === void 0)
              nodeData.attributes = {};
            nodeData.attributes[dataOne] = data[2];
            break;
          case keyData:
            nodeData.key = dataOne;
            break;
          default:
            if (nodeData.events === void 0)
              nodeData.events = {};
            if (nodeData.events[dataOne] === void 0)
              nodeData.events[dataOne] = [];
            nodeData.events[dataOne].push(data[2]);
        }
      }
    return nodeData;
  }

  // output/Flame.Html.Element/index.js
  var toNode = function(dict) {
    return dict.toNode;
  };
  var stringToNodeData = {
    toNode: function($767) {
      return singleton2(id($767));
    }
  };
  var stringToHtml = {
    toNode: function($768) {
      return singleton2(text3($768));
    }
  };
  var nodeDataToNodedata = {
    toNode: singleton2
  };
  var htmlToHtml = {
    toNode: singleton2
  };
  var createElement_ = function(tag) {
    return function(dictToNode) {
      var toNode1 = toNode(dictToNode);
      return function(children) {
        return createDatalessElementNode(tag)(toNode1(children));
      };
    };
  };
  var div_ = function(dictToNode) {
    return createElement_("div")(dictToNode);
  };
  var li_ = function(dictToNode) {
    return createElement_("li")(dictToNode);
  };
  var ul_ = function(dictToNode) {
    return createElement_("ul")(dictToNode);
  };
  var createElement$prime = function(tag) {
    return function(dictToNode) {
      var toNode1 = toNode(dictToNode);
      return function(nodeData) {
        return createSingleElementNode(tag)(toNode1(nodeData));
      };
    };
  };
  var img = function(dictToNode) {
    return createElement$prime("img")(dictToNode);
  };
  var createElement = function(tag) {
    return function(dictToNode) {
      var toNode1 = toNode(dictToNode);
      return function(dictToNode1) {
        var toNode2 = toNode(dictToNode1);
        return function(nodeData) {
          return function(children) {
            return createElementNode(tag)(toNode1(nodeData))(toNode2(children));
          };
        };
      };
    };
  };
  var div3 = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("div")(dictToNode)(dictToNode1);
    };
  };
  var figure = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("figure")(dictToNode)(dictToNode1);
    };
  };
  var h1 = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("h1")(dictToNode)(dictToNode1);
    };
  };
  var header = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("header")(dictToNode)(dictToNode1);
    };
  };
  var main = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("main")(dictToNode)(dictToNode1);
    };
  };
  var button = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("button")(dictToNode)(dictToNode1);
    };
  };
  var arrayToNodeData = function(dictToNode) {
    return {
      toNode: concatMap(toNode(dictToNode))
    };
  };

  // output/Flame.Renderer.String/foreign.js
  var reUnescapedHtml = /[&<>"']/g;
  var reHasUnescapedHtml = RegExp(reUnescapedHtml.source);

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    };
    function finalCell(head3) {
      return new ConsCell(head3, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply3) {
      return function(map10) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply3(map10(consList)(f(x)))(ys);
          };
          var go = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map10(finalCell)(f(array[array.length - 1]));
            var result = go(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map10(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Flame.Internal.Equality/foreign.js
  function compareReference_(a) {
    return function(b) {
      return a === b;
    };
  }

  // output/Flame.Internal.Equality/index.js
  var compareReference = function(a) {
    return function(a2) {
      return compareReference_(a)(a2);
    };
  };
  var modelHasChanged = function(old) {
    return function($$new4) {
      return !compareReference(old)($$new4);
    };
  };

  // output/Flame.Renderer.Internal.Dom/foreign.js
  var namespace = "http://www.w3.org/2000/svg";
  var eventPrefix = "__flame_";
  var eventPostfix = "updater";
  var textNode2 = 1;
  var elementNode2 = 2;
  var svgNode = 3;
  var fragmentNode = 4;
  var lazyNode = 5;
  var managedNode = 6;
  var nonBubblingEvents = ["focus", "blur", "scroll"];
  function start_(eventWrapper, root, updater, html) {
    return new F(eventWrapper, root, updater, html, false);
  }
  function startFrom_(eventWrapper, root, updater, html) {
    return new F(eventWrapper, root, updater, html, true);
  }
  function resume_(f, html) {
    f.resume(html);
  }
  function F(eventWrapper, root, updater, html, isDry) {
    this.eventWrapper = eventWrapper;
    this.applicationEvents = /* @__PURE__ */ new Map();
    this.root = root;
    this.updater = updater;
    this.cachedHtml = html.node === void 0 ? html : shallowCopy(html);
    if (isDry)
      this.hydrate(this.root, this.cachedHtml);
    else
      this.createAllNodes(this.root, this.cachedHtml);
  }
  F.prototype.hydrate = function(parent, html, referenceNode) {
    switch (html.nodeType) {
      case lazyNode:
        html.node = parent;
        html.rendered = html.render(html.arg);
        html.render = void 0;
        this.hydrate(parent, html.rendered);
        break;
      case textNode2:
        html.node = parent;
        break;
      case managedNode:
        this.createAllNodes(parent, html, referenceNode);
        break;
      default:
        if (html.nodeType === fragmentNode)
          html.node = document.createDocumentFragment();
        else {
          html.node = parent;
          if (html.nodeData.events !== void 0)
            this.createAllEvents(parent, html);
        }
        let htmlChildrenLength;
        if (html.text === void 0 && html.children !== void 0 && (htmlChildrenLength = html.children.length) > 0) {
          let childNodes = parent.childNodes;
          for (let i = 0, cni = 0; i < htmlChildrenLength; ++i, ++cni) {
            let c = html.children[i] = html.children[i].node === void 0 ? html.children[i] : shallowCopy(html.children[i]);
            if (childNodes[cni] === void 0)
              this.createAllNodes(parent, c);
            else {
              if (c.nodeType === fragmentNode) {
                let fragmentChildrenLength = c.children.length;
                c.node = document.createDocumentFragment();
                for (let j = 0; j < fragmentChildrenLength; ++j) {
                  let cf = c.children[j] = c.children[j].node === void 0 ? c.children[j] : shallowCopy(c.children[j]);
                  this.hydrate(childNodes[cni++], cf);
                }
                cni--;
              } else if (c.nodeType === managedNode)
                this.hydrate(parent, c, childNodes[cni]);
              else
                this.hydrate(childNodes[cni], c);
            }
          }
        }
    }
  };
  function shallowCopy(origin) {
    switch (origin.nodeType) {
      case textNode2:
        return {
          nodeType: textNode2,
          node: void 0,
          text: origin.text
        };
      case fragmentNode:
        return {
          nodeType: fragmentNode,
          node: void 0,
          children: origin.children
        };
      case lazyNode:
        return {
          nodeType: lazyNode,
          node: void 0,
          nodeData: origin.nodeData,
          render: origin.render,
          arg: origin.arg,
          rendered: void 0,
          messageMapper: origin.messageMapper
        };
      case managedNode:
        return {
          nodeType: managedNode,
          node: void 0,
          nodeData: origin.nodeData,
          createNode: origin.createNode,
          updateNode: origin.updateNode,
          arg: origin.arg,
          messageMapper: origin.messageMapper
        };
      default:
        return {
          nodeType: origin.nodeType,
          node: void 0,
          tag: origin.tag,
          nodeData: origin.nodeData,
          children: origin.children,
          text: origin.text,
          messageMapper: origin.messageMapper
        };
    }
  }
  F.prototype.createAllNodes = function(parent, html, referenceNode) {
    let node = this.createNode(html);
    if (html.text !== void 0)
      node.textContent = html.text;
    else {
      if (html.children !== void 0)
        this.createChildrenNodes(node, html.children);
      else if (html.rendered !== void 0) {
        if (html.messageMapper !== void 0)
          lazyMessageMap(html.messageMapper, html.rendered);
        if (html.rendered.text !== void 0) {
          node.textContent = html.rendered.text;
        } else if (html.rendered.children !== void 0)
          this.createChildrenNodes(node, html.rendered.children);
      }
    }
    parent.insertBefore(node, referenceNode);
  };
  F.prototype.checkCreateAllNodes = function(parent, html, referenceNode) {
    if (html.node !== void 0)
      html = shallowCopy(html);
    this.createAllNodes(parent, html, referenceNode);
    return html;
  };
  F.prototype.createChildrenNodes = function(parent, children) {
    let childrenLength = children.length;
    for (let i = 0; i < childrenLength; ++i) {
      let c = children[i] = children[i].node === void 0 ? children[i] : shallowCopy(children[i]), node = this.createNode(c);
      if (c.text !== void 0)
        node.textContent = c.text;
      else {
        if (c.children !== void 0)
          this.createChildrenNodes(node, c.children);
        else if (c.rendered !== void 0) {
          if (c.messageMapper !== void 0)
            lazyMessageMap(c.messageMapper, c.rendered);
          if (c.rendered.children !== void 0)
            this.createChildrenNodes(node, c.rendered.children);
        }
      }
      parent.appendChild(node);
    }
  };
  F.prototype.createNode = function(html) {
    switch (html.nodeType) {
      case lazyNode:
        html.rendered = html.render(html.arg);
        html.render = void 0;
        return html.node = this.createNode(html.rendered);
      case textNode2:
        return html.node = document.createTextNode(html.text);
      case elementNode2:
        return html.node = this.createElement(html);
      case svgNode:
        return html.node = this.createSvg(html);
      case fragmentNode:
        return html.node = document.createDocumentFragment();
      case managedNode:
        return html.node = this.createManagedNode(html);
    }
  };
  F.prototype.createElement = function(html) {
    let element = document.createElement(html.tag);
    this.createNodeData(element, html, false);
    return element;
  };
  F.prototype.createSvg = function(html) {
    let svg = document.createElementNS(namespace, html.tag);
    this.createNodeData(svg, html, true);
    return svg;
  };
  F.prototype.createManagedNode = function(html) {
    let node = html.createNode(html.arg)();
    html.createNode = void 0;
    this.createNodeData(node, html, node instanceof SVGElement || node.nodeName.toLowerCase() === "svg");
    return node;
  };
  F.prototype.createNodeData = function(node, html, isSvg) {
    if (html.nodeData.styles !== void 0)
      createStyles(node, html.nodeData.styles);
    if (html.nodeData.classes !== void 0 && html.nodeData.classes.length > 0)
      createClasses(node, html.nodeData.classes, isSvg);
    if (html.nodeData.attributes !== void 0)
      createAttributes(node, html.nodeData.attributes);
    if (html.nodeData.properties !== void 0)
      for (let key in html.nodeData.properties)
        node[key] = html.nodeData.properties[key];
    if (html.nodeData.events !== void 0)
      this.createAllEvents(node, html);
  };
  function createStyles(node, styles) {
    for (let key in styles)
      node.style.setProperty(key, styles[key]);
  }
  function createClasses(node, classes, isSvg) {
    let joined = classes.join(" ");
    if (isSvg)
      node.setAttribute("class", joined);
    else
      node.className = joined;
  }
  function createAttributes(node, attributes) {
    for (let key in attributes)
      node.setAttribute(key, attributes[key]);
  }
  F.prototype.createAllEvents = function(node, html) {
    for (let key in html.nodeData.events)
      this.createEvent(node, key, html);
  };
  F.prototype.createEvent = function(node, name2, html) {
    let handlers = html.nodeData.events[name2], eventKey = eventPrefix + name2;
    if (nonBubblingEvents.includes(name2)) {
      let runNonBubblingEvent = this.runNonBubblingEvent(handlers, html.messageMapper);
      node[eventKey] = runNonBubblingEvent;
      node.addEventListener(name2, runNonBubblingEvent, false);
    } else {
      node[eventKey] = handlers;
      if (html.messageMapper !== void 0)
        node[eventKey + eventPostfix] = html.messageMapper;
      let synthethic = this.applicationEvents.get(name2);
      if (synthethic === void 0) {
        let runEvent = this.runEvent.bind(this);
        this.root.addEventListener(name2, runEvent, false);
        this.applicationEvents.set(name2, {
          count: 1,
          handler: runEvent
        });
      } else
        synthethic.count++;
    }
  };
  F.prototype.runNonBubblingEvent = function(handlers, messageMapper2) {
    return function(event) {
      this.runHandlers(handlers, messageMapper2, event);
    }.bind(this);
  };
  F.prototype.runEvent = function(event) {
    let node = event.target, eventKey = eventPrefix + event.type;
    while (node !== this.root) {
      let handlers = node[eventKey];
      if (handlers !== void 0) {
        this.runHandlers(handlers, node[eventKey + eventPostfix], event);
        return;
      }
      node = node.parentNode;
    }
  };
  F.prototype.runHandlers = function(handlers, messageMapper2, event) {
    let handlersLength = handlers.length;
    for (let i = 0; i < handlersLength; ++i) {
      let h = handlers[i], maybeMessage = typeof h === "function" ? h(event)() : this.eventWrapper(h);
      this.updater(messageMapper2 === void 0 ? maybeMessage : messageMapper2(maybeMessage))();
    }
    event.stopPropagation();
  };
  F.prototype.resume = function(updatedHtml) {
    this.cachedHtml = this.updateAllNodes(this.root, this.cachedHtml, updatedHtml);
    ;
  };
  F.prototype.updateAllNodes = function(parent, currentHtml2, updatedHtml) {
    if (updatedHtml.node !== void 0)
      updatedHtml = shallowCopy(updatedHtml);
    if (currentHtml2.tag !== updatedHtml.tag || currentHtml2.nodeType !== updatedHtml.nodeType) {
      this.createAllNodes(parent, updatedHtml, currentHtml2.node);
      parent.removeChild(currentHtml2.node);
    } else {
      updatedHtml.node = currentHtml2.node;
      switch (updatedHtml.nodeType) {
        case lazyNode:
          if (updatedHtml.arg !== currentHtml2.arg) {
            updatedHtml.rendered = updatedHtml.render(updatedHtml.arg);
            if (updatedHtml.messageMapper !== void 0)
              lazyMessageMap(updatedHtml.messageMapper, updatedHtml.rendered);
            this.updateAllNodes(parent, currentHtml2.rendered, updatedHtml.rendered);
          } else
            updatedHtml.rendered = currentHtml2.rendered;
          updatedHtml.render = void 0;
          break;
        case managedNode:
          let node = updatedHtml.updateNode(currentHtml2.node)(currentHtml2.arg)(updatedHtml.arg)(), isSvg = node instanceof SVGElement || node.nodeName.toLowerCase() === "svg";
          if (node !== currentHtml2.node || node.nodeType !== currentHtml2.node.nodeType || node.nodeName !== currentHtml2.node.nodeName) {
            this.createNodeData(node, updatedHtml, isSvg);
            parent.insertBefore(node, currentHtml2.node);
            parent.removeChild(currentHtml2.node);
          } else
            this.updateNodeData(node, currentHtml2.nodeData, updatedHtml, isSvg);
          updatedHtml.node = node;
          break;
        case textNode2:
          if (updatedHtml.text !== currentHtml2.text)
            updatedHtml.node.textContent = updatedHtml.text;
          break;
        case fragmentNode:
          this.updateChildrenNodes(parent, currentHtml2, updatedHtml);
          break;
        default:
          this.updateNodeData(currentHtml2.node, currentHtml2.nodeData, updatedHtml, updatedHtml.nodeType == svgNode);
          if ((updatedHtml.text !== void 0 || updatedHtml.children === void 0 && currentHtml2.text != void 0) && !hasInnerHtml(updatedHtml.nodeData) && updatedHtml.text != currentHtml2.node.textContent)
            currentHtml2.node.textContent = updatedHtml.text;
          else
            this.updateChildrenNodes(currentHtml2.node, currentHtml2, updatedHtml);
      }
    }
    return updatedHtml;
  };
  function clearNode(node) {
    node.textContent = "";
  }
  F.prototype.updateChildrenNodes = function(parent, currentHtml2, updatedHtml) {
    let currentChildren = currentHtml2.children, updatedChildren = updatedHtml.children;
    if (currentChildren === void 0 || currentChildren.length === 0) {
      let updatedChildrenLength;
      if (updatedChildren !== void 0 && (updatedChildrenLength = updatedChildren.length) > 0) {
        if (currentHtml2.text !== void 0 || hasInnerHtml(currentHtml2.nodeData))
          clearNode(parent);
        for (let i = 0; i < updatedChildrenLength; ++i)
          updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i]);
      }
    } else if (updatedChildren === void 0 || updatedChildren.length === 0) {
      if (currentChildren !== void 0 && (currentChildren.length > 0 || currentHtml2.text !== void 0) && !hasInnerHtml(updatedHtml.nodeData))
        clearNode(parent);
    } else if (currentChildren[0].nodeData !== void 0 && currentChildren[0].nodeData.key !== void 0 && updatedChildren[0].nodeData !== void 0 && updatedChildren[0].nodeData.key !== void 0)
      this.updateKeyedChildrenNodes(parent, currentChildren, updatedChildren);
    else
      this.updateNonKeyedChildrenNodes(parent, currentChildren, updatedChildren);
  };
  function hasInnerHtml(parentNodeData) {
    return parentNodeData !== void 0 && parentNodeData.properties !== void 0 && parentNodeData.properties.innerHTML !== void 0;
  }
  F.prototype.updateKeyedChildrenNodes = function(parent, currentChildren, updatedChildren) {
    let currentStart = 0, updatedStart = 0, currentEnd = currentChildren.length - 1, updatedEnd = updatedChildren.length - 1;
    let afterNode, currentStartNode = currentChildren[currentStart].node, updatedStartNode = currentStartNode, currentEndNode = currentChildren[currentEnd].node;
    let loop = true;
    fixes:
      while (loop) {
        loop = false;
        let currentHtml2 = currentChildren[currentStart], updatedHtml = updatedChildren[updatedStart];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          updatedStartNode = currentStartNode = currentHtml2.node.nextSibling;
          currentStart++;
          updatedStart++;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentStart];
          updatedHtml = updatedChildren[updatedStart];
        }
        currentHtml2 = currentChildren[currentEnd];
        updatedHtml = updatedChildren[updatedEnd];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          afterNode = currentEndNode;
          currentEndNode = currentEndNode.previousSibling;
          currentEnd--;
          updatedEnd--;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentEnd];
          updatedHtml = updatedChildren[updatedEnd];
        }
        currentHtml2 = currentChildren[currentEnd];
        updatedHtml = updatedChildren[updatedStart];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          loop = true;
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          currentEndNode = currentHtml2.node.previousSibling;
          parent.insertBefore(currentHtml2.node, updatedStartNode);
          updatedStart++;
          currentEnd--;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentEnd];
          updatedHtml = updatedChildren[updatedStart];
        }
        currentHtml2 = currentChildren[currentStart];
        updatedHtml = updatedChildren[updatedEnd];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          loop = true;
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          parent.insertBefore(currentHtml2.node, afterNode);
          afterNode = currentHtml2.node;
          currentStart++;
          updatedEnd--;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentStart];
          updatedHtml = updatedChildren[updatedEnd];
        }
      }
    if (updatedEnd < updatedStart)
      while (currentStart <= currentEnd) {
        parent.removeChild(currentChildren[currentEnd].node);
        currentEnd--;
      }
    else if (currentEnd < currentStart)
      while (updatedStart <= updatedEnd) {
        updatedChildren[updatedStart] = this.checkCreateAllNodes(parent, updatedChildren[updatedStart], afterNode);
        updatedStart++;
      }
    else {
      let P = new Int32Array(updatedEnd + 1 - updatedStart);
      let I = /* @__PURE__ */ new Map();
      for (let i = updatedStart; i <= updatedEnd; i++) {
        P[i] = -1;
        I.set(updatedChildren[i].nodeData.key, i);
      }
      let reusingNodes = updatedStart + updatedChildren.length - 1 - updatedEnd, toRemove = [];
      for (let i = currentStart; i <= currentEnd; i++)
        if (I.has(currentChildren[i].nodeData.key)) {
          P[I.get(currentChildren[i].nodeData.key)] = i;
          reusingNodes++;
        } else
          toRemove.push(i);
      if (reusingNodes === 0) {
        parent.textContent = "";
        for (let i = updatedStart; i <= updatedEnd; i++)
          updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i]);
      } else {
        let toRemoveLength = toRemove.length;
        for (let i = 0; i < toRemoveLength; i++)
          parent.removeChild(currentChildren[toRemove[i]].node);
        let longestSeq = longestSubsequence(P, updatedStart), seqIndex = longestSeq.length - 1;
        for (let i = updatedEnd; i >= updatedStart; i--) {
          if (longestSeq[seqIndex] === i) {
            currentHtml = currentChildren[P[longestSeq[seqIndex]]];
            updatedChildren[i] = this.updateAllNodes(parent, currentHtml, updatedChildren[i]);
            afterNode = currentHtml.node;
            seqIndex--;
          } else {
            if (P[i] === -1) {
              updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i], afterNode);
              afterNode = updatedChildren[i].node;
            } else {
              currentHtml = currentChildren[P[i]];
              updatedChildren[i] = this.updateAllNodes(parent, currentHtml, updatedChildren[i]);
              parent.insertBefore(currentHtml.node, afterNode);
              afterNode = currentHtml.node;
            }
          }
        }
      }
    }
  };
  function longestSubsequence(ns, updatedStart) {
    let seq = [], is = [], l = -1, i, len, pre = new Int32Array(ns.length);
    for (i = updatedStart, len = ns.length; i < len; i++) {
      let n = ns[i];
      if (n < 0)
        continue;
      let j = findGreatestIndex(seq, n);
      if (j !== -1)
        pre[i] = is[j];
      if (j === l) {
        l++;
        seq[l] = n;
        is[l] = i;
      } else if (n < seq[j + 1]) {
        seq[j + 1] = n;
        is[j + 1] = i;
      }
    }
    for (i = is[l]; l >= 0; i = pre[i], l--)
      seq[l] = i;
    return seq;
  }
  function findGreatestIndex(seq, n) {
    let lo = -1, hi = seq.length;
    if (hi > 0 && seq[hi - 1] <= n)
      return hi - 1;
    while (hi - lo > 1) {
      let mid = Math.floor((lo + hi) / 2);
      if (seq[mid] > n)
        hi = mid;
      else
        lo = mid;
    }
    return lo;
  }
  F.prototype.updateNonKeyedChildrenNodes = function(parent, currentChildren, updatedChildren) {
    let currentChildrenLength = currentChildren.length, updatedChildrenLength = updatedChildren.length, commonLength = Math.min(currentChildrenLength, updatedChildrenLength);
    for (let i = 0; i < commonLength; ++i)
      updatedChildren[i] = this.updateAllNodes(parent, currentChildren[i], updatedChildren[i]);
    if (currentChildrenLength < updatedChildrenLength)
      for (let i = commonLength; i < updatedChildrenLength; ++i)
        updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i]);
    else if (currentChildrenLength > updatedChildrenLength)
      for (let i = commonLength; i < currentChildrenLength; ++i)
        parent.removeChild(currentChildren[i].node);
  };
  F.prototype.updateNodeData = function(node, currentNodeData, updatedHtml, isSvg) {
    updateStyles(node, currentNodeData.styles, updatedHtml.nodeData.styles);
    updateAttributes(node, currentNodeData.attributes, updatedHtml.nodeData.attributes);
    updateClasses(node, currentNodeData.classes, updatedHtml.nodeData.classes, isSvg);
    updateProperties(node, currentNodeData.properties, updatedHtml.nodeData.properties);
    this.updateEvents(node, currentNodeData.events, updatedHtml);
  };
  function updateStyles(node, currentStyles, updatedStyles) {
    if (currentStyles === void 0) {
      if (updatedStyles !== void 0)
        createStyles(node, updatedStyles);
    } else if (updatedStyles === void 0) {
      if (currentStyles !== void 0)
        node.removeAttribute("style");
    } else {
      let matchCount = 0;
      for (let key in currentStyles) {
        let current = currentStyles[key], updated = updatedStyles[key], hasUpdated = updatedStyles[key] !== void 0;
        if (hasUpdated)
          matchCount++;
        if (current !== updated)
          if (hasUpdated)
            node.style.setProperty(key, updated);
          else
            node.style.removeProperty(key);
      }
      let newKeys = Object.keys(updatedStyles), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key = newKeys[i];
        if (currentStyles[key] === void 0) {
          let updated = updatedStyles[key];
          ++matchCount;
          node.style.setProperty(key, updated);
        }
      }
    }
  }
  function updateClasses(node, currentClasses, updatedClasses, isSvg) {
    let classUpdated = updatedClasses !== void 0 && updatedClasses.length > 0;
    if (currentClasses !== void 0 && currentClasses.length > 0 && !classUpdated)
      createClasses(node, [], isSvg);
    else if (classUpdated)
      createClasses(node, updatedClasses, isSvg);
  }
  function updateAttributes(node, currentAttributes, updatedAttributes) {
    if (currentAttributes === void 0) {
      if (updatedAttributes !== void 0)
        createAttributes(node, updatedAttributes);
    } else if (updatedAttributes === void 0) {
      if (currentAttributes !== void 0)
        for (let key in currentAttributes)
          node.removeAttribute(key);
    } else {
      let matchCount = 0;
      for (let key in currentAttributes) {
        let current = currentAttributes[key], updated = updatedAttributes[key], hasUpdated = updated !== void 0;
        if (hasUpdated)
          matchCount++;
        if (current !== updated)
          if (hasUpdated)
            node.setAttribute(key, updated);
          else
            node.removeAttribute(key);
      }
      let newKeys = Object.keys(updatedAttributes), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key = newKeys[i];
        if (currentAttributes[key] === void 0) {
          let updated = updatedAttributes[key];
          ++matchCount;
          node.setAttribute(key, updated);
        }
      }
    }
  }
  function updateProperties(node, currentProperties, updatedProperties) {
    let addAll = currentProperties === void 0, removeAll = updatedProperties === void 0;
    if (addAll) {
      if (!removeAll)
        for (let key in updatedProperties)
          node[key] = updatedProperties[key];
    } else if (removeAll) {
      if (!addAll)
        for (let key in currentProperties)
          node.removeAttribute(key);
    } else {
      let matchCount = 0;
      for (let key in currentProperties) {
        let current = currentProperties[key], updated = updatedProperties[key], hasUpdated = updated !== void 0;
        if (hasUpdated)
          matchCount++;
        if (current !== updated)
          if (hasUpdated)
            node[key] = updated;
          else
            node.removeAttribute(key);
      }
      let newKeys = Object.keys(updatedProperties), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key = newKeys[i];
        if (currentProperties[key] === void 0) {
          let updated = updatedProperties[key];
          ++matchCount;
          node[key] = updated;
        }
      }
    }
  }
  F.prototype.updateEvents = function(node, currentEvents, updatedHtml) {
    let updatedEvents = updatedHtml.nodeData.events;
    if (currentEvents === void 0) {
      if (updatedEvents !== void 0)
        this.createAllEvents(node, updatedHtml);
    } else if (updatedEvents === void 0) {
      if (currentEvents !== void 0)
        for (let key in currentEvents)
          this.removeEvent(node, key);
    } else {
      let matchCount = 0;
      for (let key in currentEvents) {
        let current = currentEvents[key], updated = updatedEvents[key], hasUpdated = false;
        if (updated === void 0)
          this.removeEvent(node, key);
        else {
          let currentLength = current.length, updatedLength = updated.length;
          if (currentLength != updatedLength)
            hasUpdated = true;
          else {
            for (let i = 0; i < currentLength; ++i)
              if (current[i] != updated[i]) {
                hasUpdated = true;
                break;
              }
          }
        }
        if (hasUpdated) {
          matchCount++;
          this.removeEvent(node, key);
          this.createEvent(node, key, updatedHtml);
        }
      }
      let newKeys = Object.keys(updatedEvents), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key = newKeys[i];
        if (currentEvents[key] === void 0) {
          ++matchCount;
          this.createEvent(node, key, updatedHtml);
        }
      }
    }
  };
  F.prototype.removeEvent = function(node, name2) {
    let eventKey = eventPrefix + name2;
    if (nonBubblingEvents.includes(name2)) {
      let runNonBubblingEvent = node[eventKey];
      node.removeEventListener(name2, runNonBubblingEvent, false);
    } else {
      let count = --this.applicationEvents.get(name2).count;
      if (count === 0) {
        this.root.removeEventListener(name2, this.applicationEvents.get(name2).handler, false);
        this.applicationEvents.delete(name2);
      }
    }
    node[eventKey + eventPostfix] = void 0;
    node[eventKey] = void 0;
  };
  function lazyMessageMap(mapper, html) {
    html.messageMapper = mapper;
    if (html.children !== void 0 && html.children.length > 0)
      for (let i = 0; i < html.children.length; ++i)
        lazyMessageMap(mapper, html.children[i]);
  }

  // output/Flame.Renderer.Internal.Dom/index.js
  var pure4 = /* @__PURE__ */ pure(applicativeEffect);
  var resume = /* @__PURE__ */ runEffectFn2(resume_);
  var maybeUpdater = function(updater) {
    return function(v) {
      if (v instanceof Just) {
        return updater(v.value0);
      }
      ;
      return pure4(unit);
    };
  };
  var start = function(parent) {
    return function(updater) {
      return runEffectFn4(start_)(Just.create)(parent)(maybeUpdater(updater));
    };
  };
  var startFrom = function(parent) {
    return function(updater) {
      return runEffectFn4(startFrom_)(Just.create)(parent)(maybeUpdater(updater));
    };
  };

  // output/Flame.Subscription.Internal.Listener/foreign.js
  var applicationIds = /* @__PURE__ */ new Set();
  function checkApplicationId_(id3) {
    if (applicationIds.has(id3))
      throw `Error mounting application: id ${id3} already registered!`;
    applicationIds.add(id3);
  }

  // output/Flame.Types/index.js
  var Window = /* @__PURE__ */ function() {
    function Window2() {
    }
    ;
    Window2.value = new Window2();
    return Window2;
  }();
  var Document = /* @__PURE__ */ function() {
    function Document2() {
    }
    ;
    Document2.value = new Document2();
    return Document2;
  }();
  var Custom = /* @__PURE__ */ function() {
    function Custom2() {
    }
    ;
    Custom2.value = new Custom2();
    return Custom2;
  }();

  // output/Flame.Subscription.Internal.Listener/index.js
  var createSubscription = function(updater) {
    return function(v) {
      if (v.value0 instanceof Window) {
        return createWindowListener(v.value1.value0)(function($13) {
          return updater(v.value1.value1.value0($13));
        });
      }
      ;
      if (v.value0 instanceof Document) {
        return createDocumentListener(v.value1.value0)(function($14) {
          return updater(v.value1.value1.value0($14));
        });
      }
      ;
      if (v.value0 instanceof Custom) {
        return createCustomListener(v.value1.value0)(function($15) {
          return updater(v.value1.value1.value0($15));
        });
      }
      ;
      throw new Error("Failed pattern match at Flame.Subscription.Internal.Listener (line 31, column 83 - line 34, column 76): " + [v.value0.constructor.name]);
    };
  };
  var checkApplicationId = /* @__PURE__ */ runEffectFn1(checkApplicationId_);
  var createMessageListener = function(appId) {
    return function(updater) {
      return function __do() {
        checkApplicationId(appId)();
        return createCustomListener(appId)(function($16) {
          return updater(unsafeFromForeign($16));
        })();
      };
    };
  };

  // output/Flame.Application.EffectList/index.js
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableArray);
  var pure5 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var map8 = /* @__PURE__ */ map(functorMaybe);
  var showId = function(dictShow) {
    var show4 = show(dictShow);
    return function(v) {
      return show4(v);
    };
  };
  var run3 = function(parent) {
    return function(isResumed) {
      return function(appId) {
        return function(v) {
          return function __do() {
            var modelState = $$new(v.init.value0)();
            var renderingState = $$new(21)();
            var render2 = function(model) {
              return function __do2() {
                var rendering2 = read(renderingState)();
                resume(rendering2)(v.view(model))();
                return write(model)(modelState)();
              };
            };
            var runUpdate = function(message2) {
              return function __do2() {
                var currentModel = read(modelState)();
                var v1 = v.update(currentModel)(message2);
                when2(modelHasChanged(currentModel)(v1.value0))(render2(v1.value0))();
                return runMessages(v1.value1)();
              };
            };
            var runMessages = function(affs) {
              return for_2(affs)(runAff_(function(v1) {
                if (v1 instanceof Left) {
                  return log(message(v1.value0));
                }
                ;
                if (v1 instanceof Right && v1.value0 instanceof Just) {
                  return runUpdate(v1.value0.value0);
                }
                ;
                return pure5(unit);
              }));
            };
            var rendering = function() {
              if (isResumed) {
                return startFrom(parent)(runUpdate)(v.view(v.init.value0))();
              }
              ;
              return start(parent)(runUpdate)(v.view(v.init.value0))();
            }();
            write(rendering)(renderingState)();
            runMessages(v.init.value1)();
            (function() {
              if (appId instanceof Nothing) {
                return unit;
              }
              ;
              if (appId instanceof Just) {
                return createMessageListener(appId.value0)(runUpdate)();
              }
              ;
              throw new Error("Failed pattern match at Flame.Application.EffectList (line 142, column 7 - line 144, column 63): " + [appId.constructor.name]);
            })();
            return traverse_2(createSubscription(runUpdate))(v.subscribe)();
          };
        };
      };
    };
  };
  var noAppId = /* @__PURE__ */ function() {
    return Nothing.value;
  }();
  var mountWith = function(dictShow) {
    var showId1 = showId(dictShow);
    return function(v) {
      return function(appId) {
        return function(application) {
          return function __do() {
            var maybeElement = querySelector(v)();
            if (maybeElement instanceof Just) {
              return run3(maybeElement.value0)(false)(map8(showId1)(appId))(application)();
            }
            ;
            if (maybeElement instanceof Nothing) {
              return $$throw("Error mounting application")();
            }
            ;
            throw new Error("Failed pattern match at Flame.Application.EffectList (line 102, column 7 - line 104, column 63): " + [maybeElement.constructor.name]);
          };
        };
      };
    };
  };
  var mountWith1 = /* @__PURE__ */ mountWith(showUnit);
  var mount_ = function(selector) {
    return mountWith1(selector)(noAppId);
  };

  // output/Flame.Html.Event/foreign.js
  var messageEventData = 5;
  function createEvent_(name2) {
    return function(message2) {
      return [messageEventData, name2, message2];
    };
  }

  // output/Flame.Html.Event/index.js
  var createEvent = function(name2) {
    return function(message2) {
      return createEvent_(name2)(message2);
    };
  };
  var onClick = /* @__PURE__ */ createEvent("click");

  // output/Yoga.JSON/foreign.js
  function reviver(key, value) {
    if (key === "big") {
      return BigInt(value);
    }
    return value;
  }
  var _parseJSON = (payload) => JSON.parse(payload, reviver);
  var _undefined = void 0;
  function replacer(key, value) {
    if (key === "big") {
      return value.toString();
    }
    return value;
  }
  var _unsafeStringify = (data) => JSON.stringify(data, replacer);

  // output/Data.BigInt/foreign.js
  var import_big_integer = __toESM(require_BigInteger(), 1);

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value) {
    return value == null ? f : s(value[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail2 = fail(dictMonad);
    var pure8 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value) {
        return unsafeReadPropImpl(fail2(new TypeMismatch("object", typeOf(value))), pure8, k, value);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Record.Builder/foreign.js
  function copyRecord(rec) {
    var copy = {};
    for (var key in rec) {
      if ({}.hasOwnProperty.call(rec, key)) {
        copy[key] = rec[key];
      }
    }
    return copy;
  }
  function unsafeInsert(l) {
    return function(a) {
      return function(rec) {
        rec[l] = a;
        return rec;
      };
    };
  }

  // output/Record.Builder/index.js
  var semigroupoidBuilder = semigroupoidFn;
  var insert5 = function() {
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(l) {
          return function(a) {
            return function(r1) {
              return unsafeInsert(reflectSymbol2(l))(a)(r1);
            };
          };
        };
      };
    };
  };
  var categoryBuilder = categoryFn;
  var build = function(v) {
    return function(r1) {
      return v(copyRecord(r1));
    };
  };

  // output/Yoga.JSON/index.js
  var identity6 = /* @__PURE__ */ identity(categoryBuilder);
  var readString3 = /* @__PURE__ */ readString(monadIdentity);
  var readNumber2 = /* @__PURE__ */ readNumber(monadIdentity);
  var applicativeExceptT2 = /* @__PURE__ */ applicativeExceptT(monadIdentity);
  var pure6 = /* @__PURE__ */ pure(applicativeExceptT2);
  var compose1 = /* @__PURE__ */ compose(semigroupoidBuilder);
  var insert6 = /* @__PURE__ */ insert5()();
  var append3 = /* @__PURE__ */ append(semigroupNonEmptyList);
  var except2 = /* @__PURE__ */ except(applicativeIdentity);
  var functorExceptT2 = /* @__PURE__ */ functorExceptT(functorIdentity);
  var map1 = /* @__PURE__ */ map(functorExceptT2);
  var bindExceptT2 = /* @__PURE__ */ bindExceptT(monadIdentity);
  var pure1 = /* @__PURE__ */ pure(applicativeNonEmptyList);
  var map22 = /* @__PURE__ */ map(functorNonEmptyList);
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindExceptT2);
  var lmap2 = /* @__PURE__ */ lmap(bifunctorEither);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindExceptT2);
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var writeForeignString = {
    writeImpl: unsafeToForeign
  };
  var writeForeignNumber = {
    writeImpl: unsafeToForeign
  };
  var writeForeignFieldsNilRowR = {
    writeImplFields: function(v) {
      return function(v1) {
        return identity6;
      };
    }
  };
  var readForeignString = {
    readImpl: readString3
  };
  var readForeignNumber = {
    readImpl: readNumber2
  };
  var readForeignFieldsNilRowRo = {
    getFields: function(v) {
      return function(v1) {
        return pure6(identity6);
      };
    }
  };
  var writeImplFields = function(dict) {
    return dict.writeImplFields;
  };
  var writeForeignRecord = function() {
    return function(dictWriteForeignFields) {
      var writeImplFields1 = writeImplFields(dictWriteForeignFields);
      return {
        writeImpl: function(rec) {
          var steps = writeImplFields1($$Proxy.value)(rec);
          return unsafeToForeign(build(steps)({}));
        }
      };
    };
  };
  var writeImpl = function(dict) {
    return dict.writeImpl;
  };
  var writeJSON = function(dictWriteForeign) {
    var $390 = writeImpl(dictWriteForeign);
    return function($391) {
      return _unsafeStringify($390($391));
    };
  };
  var writeForeignFieldsCons = function(dictIsSymbol) {
    var get3 = get2(dictIsSymbol)();
    var insert32 = insert6(dictIsSymbol);
    return function(dictWriteForeign) {
      var writeImpl4 = writeImpl(dictWriteForeign);
      return function(dictWriteForeignFields) {
        var writeImplFields1 = writeImplFields(dictWriteForeignFields);
        return function() {
          return function() {
            return function() {
              return {
                writeImplFields: function(v) {
                  return function(rec) {
                    var rest = writeImplFields1($$Proxy.value)(rec);
                    var value = writeImpl4(get3($$Proxy.value)(rec));
                    var result = compose1(insert32($$Proxy.value)(value))(rest);
                    return result;
                  };
                }
              };
            };
          };
        };
      };
    };
  };
  var $$undefined = _undefined;
  var writeForeignMaybe = function(dictWriteForeign) {
    return {
      writeImpl: maybe($$undefined)(writeImpl(dictWriteForeign))
    };
  };
  var readImpl = function(dict) {
    return dict.readImpl;
  };
  var readForeignMaybe = function(dictReadForeign) {
    return {
      readImpl: function() {
        var readNullOrUndefined = function(v) {
          return function(value) {
            if (isNull(value) || isUndefined(value)) {
              return pure6(Nothing.value);
            }
            ;
            return map1(Just.create)(v(value));
          };
        };
        return readNullOrUndefined(readImpl(dictReadForeign));
      }()
    };
  };
  var parseJSON = /* @__PURE__ */ function() {
    var $440 = lmap2(function($443) {
      return pure1(ForeignError.create(message($443)));
    });
    var $441 = runEffectFn1(_parseJSON);
    return function($442) {
      return ExceptT(Identity($440(unsafePerformEffect($$try($441($442))))));
    };
  }();
  var readJSON = function(dictReadForeign) {
    var $444 = composeKleisliFlipped2(readImpl(dictReadForeign))(parseJSON);
    return function($445) {
      return runExcept($444($445));
    };
  };
  var readJSON_ = function(dictReadForeign) {
    var $446 = readJSON(dictReadForeign);
    return function($447) {
      return hush($446($447));
    };
  };
  var getFields = function(dict) {
    return dict.getFields;
  };
  var readForeignFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    var insert32 = insert6(dictIsSymbol);
    return function(dictReadForeign) {
      var readImpl3 = readImpl(dictReadForeign);
      return function(dictReadForeignFields) {
        var getFields1 = getFields(dictReadForeignFields);
        return function() {
          return function() {
            return {
              getFields: function(v) {
                return function(obj) {
                  var rest = getFields1($$Proxy.value)(obj);
                  var name2 = reflectSymbol2($$Proxy.value);
                  var enrichErrorWithPropName = withExcept(map22(ErrorAtProperty.create(name2)));
                  var value = enrichErrorWithPropName(bindFlipped3(readImpl3)(readProp2(name2)(obj)));
                  var first = map1(insert32($$Proxy.value))(value);
                  return except2(function() {
                    var v1 = runExcept(rest);
                    var v2 = runExcept(first);
                    if (v2 instanceof Right && v1 instanceof Right) {
                      return new Right(compose1(v2.value0)(v1.value0));
                    }
                    ;
                    if (v2 instanceof Left && v1 instanceof Left) {
                      return new Left(append3(v2.value0)(v1.value0));
                    }
                    ;
                    if (v2 instanceof Right && v1 instanceof Left) {
                      return new Left(v1.value0);
                    }
                    ;
                    if (v2 instanceof Left && v1 instanceof Right) {
                      return new Left(v2.value0);
                    }
                    ;
                    throw new Error("Failed pattern match at Yoga.JSON (line 338, column 5 - line 342, column 34): " + [v2.constructor.name, v1.constructor.name]);
                  }());
                };
              }
            };
          };
        };
      };
    };
  };
  var readForeignRecord = function() {
    return function(dictReadForeignFields) {
      var getFields1 = getFields(dictReadForeignFields);
      return {
        readImpl: function(o) {
          return map1(flip(build)({}))(getFields1($$Proxy.value)(o));
        }
      };
    };
  };

  // output/Main/index.js
  var ratingIsSymbol = {
    reflectSymbol: function() {
      return "rating";
    }
  };
  var priceIsSymbol = {
    reflectSymbol: function() {
      return "price";
    }
  };
  var nameIsSymbol = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var imageLinkIsSymbol = {
    reflectSymbol: function() {
      return "imageLink";
    }
  };
  var idIsSymbol = {
    reflectSymbol: function() {
      return "id";
    }
  };
  var eq2 = /* @__PURE__ */ eq(/* @__PURE__ */ eqRec()(/* @__PURE__ */ eqRowCons(/* @__PURE__ */ eqRowCons(/* @__PURE__ */ eqRowCons(/* @__PURE__ */ eqRowCons(/* @__PURE__ */ eqRowCons(eqRowNil)()(ratingIsSymbol)(eqNumber))()(priceIsSymbol)(/* @__PURE__ */ eqMaybe(eqString)))()(nameIsSymbol)(eqString))()(imageLinkIsSymbol)(eqString))()(idIsSymbol)(eqString)));
  var discard2 = /* @__PURE__ */ discard(discardUnit)(bindAff);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bind4 = /* @__PURE__ */ bind(bindAff);
  var fetch4 = /* @__PURE__ */ fetch3()();
  var toCoreRequestOptionsRowRo2 = /* @__PURE__ */ toCoreRequestOptionsRowRo()();
  var toCoreRequestOptionsHelpe12 = /* @__PURE__ */ toCoreRequestOptionsHelpe1(/* @__PURE__ */ toCoreRequestOptionsConve7())()()()({
    reflectSymbol: function() {
      return "headers";
    }
  })(/* @__PURE__ */ toCoreRequestOptionsHelpe1(toCoreRequestOptionsConve8)()()()({
    reflectSymbol: function() {
      return "method";
    }
  })(toCoreRequestOptionsHelpe)()())()();
  var fetch1 = /* @__PURE__ */ fetch4(/* @__PURE__ */ toCoreRequestOptionsRowRo2(/* @__PURE__ */ toCoreRequestOptionsHelpe1(/* @__PURE__ */ toCoreRequestOptionsConve6(toRequestBodyString))()()()({
    reflectSymbol: function() {
      return "body";
    }
  })(toCoreRequestOptionsHelpe12)()()));
  var map9 = /* @__PURE__ */ map(functorAff);
  var pure7 = /* @__PURE__ */ pure(applicativeAff);
  var writeForeignRecord2 = /* @__PURE__ */ writeForeignRecord();
  var writeJSON2 = /* @__PURE__ */ writeJSON(/* @__PURE__ */ writeForeignRecord2(/* @__PURE__ */ writeForeignFieldsCons(idIsSymbol)(writeForeignString)(/* @__PURE__ */ writeForeignFieldsCons({
    reflectSymbol: function() {
      return "likedResturant";
    }
  })(/* @__PURE__ */ writeForeignMaybe(writeForeignString))(writeForeignFieldsNilRowR)()()())()()()));
  var arrayToNodeData2 = /* @__PURE__ */ arrayToNodeData(nodeDataToNodedata);
  var div4 = /* @__PURE__ */ div3(arrayToNodeData2);
  var arrayToNodeData1 = /* @__PURE__ */ arrayToNodeData(htmlToHtml);
  var div1 = /* @__PURE__ */ div4(arrayToNodeData1);
  var class$prime2 = /* @__PURE__ */ class$prime(stringClassList);
  var figure2 = /* @__PURE__ */ figure(arrayToNodeData2)(arrayToNodeData1);
  var img2 = /* @__PURE__ */ img(arrayToNodeData2);
  var header1 = /* @__PURE__ */ header(arrayToNodeData2)(arrayToNodeData1);
  var h12 = /* @__PURE__ */ h1(arrayToNodeData2)(stringToHtml);
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorEffect);
  var mempty3 = /* @__PURE__ */ mempty(monoidCanceler);
  var readJSON_2 = /* @__PURE__ */ readJSON_(readForeignString);
  var button2 = /* @__PURE__ */ button(arrayToNodeData2)(stringToHtml);
  var main1 = /* @__PURE__ */ main(stringToNodeData)(arrayToNodeData1);
  var div_2 = /* @__PURE__ */ div_(arrayToNodeData1);
  var div22 = /* @__PURE__ */ div4(htmlToHtml);
  var ul_2 = /* @__PURE__ */ ul_(arrayToNodeData1);
  var show3 = /* @__PURE__ */ show(showNumber);
  var map12 = /* @__PURE__ */ map(functorArray);
  var li_2 = /* @__PURE__ */ li_(stringToHtml);
  var writeJSON1 = /* @__PURE__ */ writeJSON(/* @__PURE__ */ writeForeignRecord2(/* @__PURE__ */ writeForeignFieldsCons({
    reflectSymbol: function() {
      return "latitude";
    }
  })(writeForeignNumber)(/* @__PURE__ */ writeForeignFieldsCons({
    reflectSymbol: function() {
      return "longitude";
    }
  })(writeForeignNumber)(writeForeignFieldsNilRowR)()()())()()()));
  var showMaybe2 = /* @__PURE__ */ showMaybe(showString);
  var show1 = /* @__PURE__ */ show(showMaybe2);
  var show23 = /* @__PURE__ */ show(showString);
  var applySecond2 = /* @__PURE__ */ applySecond(applyAff);
  var show32 = /* @__PURE__ */ show(/* @__PURE__ */ showMaybe(/* @__PURE__ */ showRecord()()(/* @__PURE__ */ showRecordFieldsCons(idIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(imageLinkIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(nameIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(priceIsSymbol)(/* @__PURE__ */ showRecordFieldsConsNil(ratingIsSymbol)(showNumber))(showMaybe2))(showString))(showString))(showString))));
  var StartSwiping = /* @__PURE__ */ function() {
    function StartSwiping2() {
    }
    ;
    StartSwiping2.value = new StartSwiping2();
    return StartSwiping2;
  }();
  var Like = /* @__PURE__ */ function() {
    function Like2() {
    }
    ;
    Like2.value = new Like2();
    return Like2;
  }();
  var Dislike = /* @__PURE__ */ function() {
    function Dislike2() {
    }
    ;
    Dislike2.value = new Dislike2();
    return Dislike2;
  }();
  var FailedToLoad = /* @__PURE__ */ function() {
    function FailedToLoad2(value0) {
      this.value0 = value0;
    }
    ;
    FailedToLoad2.create = function(value0) {
      return new FailedToLoad2(value0);
    };
    return FailedToLoad2;
  }();
  var NextResturant = /* @__PURE__ */ function() {
    function NextResturant2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NextResturant2.create = function(value0) {
      return function(value1) {
        return new NextResturant2(value0, value1);
      };
    };
    return NextResturant2;
  }();
  var Matched = /* @__PURE__ */ function() {
    function Matched2(value0) {
      this.value0 = value0;
    }
    ;
    Matched2.create = function(value0) {
      return new Matched2(value0);
    };
    return Matched2;
  }();
  var Finish = /* @__PURE__ */ function() {
    function Finish2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Finish2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Finish2(value0, value1, value2);
        };
      };
    };
    return Finish2;
  }();
  var QR = /* @__PURE__ */ function() {
    function QR2() {
    }
    ;
    QR2.value = new QR2();
    return QR2;
  }();
  var Swiping = /* @__PURE__ */ function() {
    function Swiping2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Swiping2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Swiping2(value0, value1, value2);
        };
      };
    };
    return Swiping2;
  }();
  var Match = /* @__PURE__ */ function() {
    function Match2(value0) {
      this.value0 = value0;
    }
    ;
    Match2.create = function(value0) {
      return new Match2(value0);
    };
    return Match2;
  }();
  var ServerError = /* @__PURE__ */ function() {
    function ServerError2(value0) {
      this.value0 = value0;
    }
    ;
    ServerError2.create = function(value0) {
      return new ServerError2(value0);
    };
    return ServerError2;
  }();
  var updateUrl = "http://localhost:5000/liked";
  var skipDuplicateResturant = function(v) {
    return function(v1) {
      if (v instanceof Swiping) {
        var $196 = eq2(v.value0)(v1);
        if ($196) {
          return new NextResturant(v.value1, v.value2);
        }
        ;
        return new Finish(v1, v.value1, v.value2);
      }
      ;
      return new FailedToLoad("Attempted to determin resturant when no resturant is set");
    };
  };
  var readApi = /* @__PURE__ */ readJSON_(/* @__PURE__ */ readForeignRecord()(/* @__PURE__ */ readForeignFieldsCons(idIsSymbol)(readForeignString)(/* @__PURE__ */ readForeignFieldsCons(imageLinkIsSymbol)(readForeignString)(/* @__PURE__ */ readForeignFieldsCons(nameIsSymbol)(readForeignString)(/* @__PURE__ */ readForeignFieldsCons(priceIsSymbol)(/* @__PURE__ */ readForeignMaybe(readForeignString))(/* @__PURE__ */ readForeignFieldsCons(ratingIsSymbol)(readForeignNumber)(readForeignFieldsNilRowRo)()())()())()())()())()()));
  var post = function(v) {
    return function(url2) {
      return discard2(liftEffect5(log("post")))(function() {
        return bind4(fetch1(url2)({
          method: POST.value,
          headers: {
            "Content-Type": "application/json"
          },
          body: v
        }))(function(v1) {
          if (v1.status === 200) {
            return map9(readApi)(v1.text);
          }
          ;
          return pure7(Nothing.value);
        });
      });
    };
  };
  var sendLiked = function(id3) {
    return function(liked_) {
      var des = {
        id: id3,
        likedResturant: liked_
      };
      return post(writeJSON2(des))(updateUrl);
    };
  };
  var init3 = /* @__PURE__ */ function() {
    return QR.value;
  }();
  var image = function(link) {
    return div1([class$prime2("py-5 flex flex-col items-center justify-center")])([figure2([class$prime2("flex flex-row justify-center")])([img2([class$prime2("is-cropped rounded overflow-hidden shadow-lg"), src(link), alt3("Resturant Image")])])]);
  };
  var header2 = function(text4) {
    return header1([class$prime2("relative flex items-center shadow-md justify-center mx-2 rounded-lg")])([h12([class$prime2("text-3xl lg:text-3xl font-bold mb-2")])(text4)]);
  };
  var geolocation = function(dictMonadAff) {
    return liftAff(dictMonadAff)(makeAff(function(cb) {
      return voidLeft2(_geolocation(Right.create)(Left.create)(cb))(mempty3);
    }));
  };
  var geolocation1 = /* @__PURE__ */ geolocation(monadAffAff);
  var createSession = /* @__PURE__ */ function() {
    return bind4(fetch4(toCoreRequestOptionsRowRo2(toCoreRequestOptionsHelpe12))("http://localhost:5000/newsession")({
      method: GET.value,
      headers: {
        "Content-Type": "application/json"
      }
    }))(function(v) {
      if (v.status === 200) {
        return map9(readJSON_2)(v.text);
      }
      ;
      return pure7(Nothing.value);
    });
  }();
  var btn = function(m) {
    return function(t) {
      return function(c) {
        var attrs = [class$prime2("block py-2 px-10 fill text-white shadow-md rounded hover:shadow-lg"), onClick(m)];
        return button2(function() {
          if (c instanceof Just) {
            return cons(class$prime2(c.value0))(attrs);
          }
          ;
          if (c instanceof Nothing) {
            return attrs;
          }
          ;
          throw new Error("Failed pattern match at Main (line 245, column 7 - line 247, column 25): " + [c.constructor.name]);
        }())(t);
      };
    };
  };
  var view = function(v) {
    if (v instanceof ServerError) {
      return main1("main")([text3("Error: " + v.value0)]);
    }
    ;
    if (v instanceof QR) {
      return main1("main")([div1([class$prime2("flex flex-col items-center justify-center ")])([text3("Replace with QR code"), div_2([btn(StartSwiping.value)("Start Swiping")(new Just("bg-gray-900 hover:bg-gray-800"))])])]);
    }
    ;
    if (v instanceof Match) {
      return main1("main")([text3("Yay you got a match on" + v.value0.name)]);
    }
    ;
    if (v instanceof Swiping) {
      return main1("main")([div1([class$prime2("my-0 fill bg-gray-100")])([div1([class$prime2("justify-center pt-3")])([div_2([header2(v.value0.name), image(v.value0.imageLink)])]), div_2([div1([class$prime2("bg-white py-2 shadow-lg px-2 rounded-lg mx-2")])([div1([class$prime2("fill flex items-center justify-between mb-3lcontainer mx-auto rounded")])([div22([class$prime2("items-left")])(btn(Dislike.value)("-")(new Just("bg-red-900 hover:bg-red-800"))), ul_2(function() {
        var ratings = " Rating: " + show3(v.value0.rating);
        return map12(li_2)(function() {
          if (v.value0.price instanceof Nothing) {
            return [ratings];
          }
          ;
          if (v.value0.price instanceof Just) {
            return [" Price: " + v.value0.price.value0, ratings];
          }
          ;
          throw new Error("Failed pattern match at Main (line 286, column 38 - line 288, column 69): " + [v.value0.price.constructor.name]);
        }());
      }()), div22([class$prime2("right-0")])(btn(Like.value)("+")(new Just("bg-green-900 hover:bg-green-800")))])])])])]);
    }
    ;
    throw new Error("Failed pattern match at Main (line 252, column 1 - line 252, column 30): " + [v.constructor.name]);
  };
  var apiUrl = "http://localhost:5000/gavin";
  var getResturant = function(loc) {
    return post(writeJSON1(loc))(apiUrl);
  };
  var update = function(model) {
    return function(v) {
      if (v instanceof FailedToLoad) {
        return new Tuple(new ServerError(v.value0), []);
      }
      ;
      if (v instanceof Finish) {
        return new Tuple(new Swiping(v.value0, v.value1, v.value2), []);
      }
      ;
      if (v instanceof Matched) {
        return new Tuple(new Match(v.value0), []);
      }
      ;
      if (v instanceof StartSwiping) {
        return new Tuple(model, [map9(Just.create)(bind4(createSession)(function(sess) {
          return discard2(liftEffect5(log(show1(sess))))(function() {
            if (sess instanceof Just) {
              return bind4(geolocation1)(function(loc) {
                return pure7(new NextResturant(loc, sess.value0));
              });
            }
            ;
            if (sess instanceof Nothing) {
              return pure7(StartSwiping.value);
            }
            ;
            throw new Error("Failed pattern match at Main (line 159, column 9 - line 163, column 41): " + [sess.constructor.name]);
          });
        }))]);
      }
      ;
      if (v instanceof Like) {
        return new Tuple(model, [map9(Just.create)(function() {
          if (model instanceof Swiping) {
            return bind4(sendLiked(model.value2)(new Just(model.value0.id)))(function(maybeMatch) {
              return discard2(liftEffect5(log("liked " + show23(model.value0.id))))(function() {
                return pure7(function() {
                  if (maybeMatch instanceof Just) {
                    return new Matched(maybeMatch.value0);
                  }
                  ;
                  if (maybeMatch instanceof Nothing) {
                    return new NextResturant(model.value1, model.value2);
                  }
                  ;
                  throw new Error("Failed pattern match at Main (line 174, column 22 - line 176, column 47): " + [maybeMatch.constructor.name]);
                }());
              });
            });
          }
          ;
          return pure7(new FailedToLoad("Attempted to like while not swiping"));
        }())]);
      }
      ;
      if (v instanceof Dislike) {
        return new Tuple(model, [map9(Just.create)(function() {
          if (model instanceof Swiping) {
            return applySecond2(sendLiked(model.value2)(Nothing.value))(pure7(new NextResturant(model.value1, model.value2)));
          }
          ;
          return pure7(new FailedToLoad("Attempted to dislike while not swiping"));
        }())]);
      }
      ;
      if (v instanceof NextResturant) {
        return new Tuple(model, [map9(Just.create)(bind4(getResturant(v.value0))(function(maybeRes) {
          return discard2(liftEffect5(log("nextresturant" + show32(maybeRes))))(function() {
            return pure7(function() {
              if (maybeRes instanceof Nothing) {
                return new FailedToLoad("API did not return a resturant");
              }
              ;
              if (maybeRes instanceof Just) {
                if (model instanceof QR) {
                  return new Finish(maybeRes.value0, v.value0, v.value1);
                }
                ;
                if (model instanceof Swiping) {
                  return skipDuplicateResturant(model)(maybeRes.value0);
                }
                ;
                return new FailedToLoad("Attempted to find next resturant when not swiping or starting a session");
              }
              ;
              throw new Error("Failed pattern match at Main (line 196, column 17 - line 202, column 96): " + [maybeRes.constructor.name]);
            }());
          });
        }))]);
      }
      ;
      throw new Error("Failed pattern match at Main (line 149, column 16 - line 203, column 10): " + [v.constructor.name]);
    };
  };
  var main2 = /* @__PURE__ */ function() {
    return mount_("body")({
      init: new Tuple(init3, []),
      subscribe: [],
      update,
      view
    });
  }();

  // <stdin>
  main2();
})();
