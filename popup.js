"use strict";

class GenQRCode {
	constructor(message) {
		const Type = GenQRCodeType;
		const Matrix = GenQRCodeMatrix;
		this.Type = Type;
		this.Matrix = Matrix;
		this.message = message;
		this.autoEncodeMessage = [];
		this.autoEncodeImage = [];
	}

	autoEncode(option, scale) {
		const autoencmsg = this.Type.autoEncodeMessage(this.message, option);
		const image = [];
		for (let level = 0; level < autoencmsg.length; level++) {
			const encode = autoencmsg[level];
			if (encode == undefined)
				continue;
			encode.image = new this.Matrix(encode.type).createBestImage(level, encode.data);
			encode.gif = new GenQRCodeGIF(encode.image.matrix, scale);
			encode.base64gif = GenQRCodeBase64.encode(encode.gif.binary);
		}
		this.autoEncodeMessage = autoencmsg;
		return autoencmsg;
	}
}

class GenQRCodeType {
	static SizeListMicro  = [...Array(4)].map((_, i) => (11 + (i << 1)));
	static SizeListNormal = [...Array(40)].map((_, i) => (21 + (i << 2)));
	static SizeListAll = (GenQRCodeType.SizeListMicro.concat(GenQRCodeType.SizeListNormal));

	static TypeSizeMap/*[micro]*/ = {
		true : [...Array( 4+1)].map((_, i) => (i ? ( 9 + (i << 1)) : undefined)),
		false: [...Array(40+1)].map((_, i) => (i ? (17 + (i << 2)) : undefined)),
	}

	static SizeTypeMap = function() {
		const r = new Map();
		for (let size of GenQRCodeType.SizeListAll) {
			const micro = (size < 21);
			const type = ((micro) ? ((size - 9) >> 1) : ((size - 17) >> 2));
			r.set(size, { type: type, micro: micro });
		}
		return r;
	}();

	static BlockTable = [
		[[ 50,  450,  3522], [  1,   4,  84,  80,  64,  0,  5,  80,  5,  80], []],
		[[100,  919,  2076], [ 65,  88, 129,  22, 148, 66, 84,  81,  5,  85], []],
		[[135, 1353,  1712], [ 17, 102,  70, 198,  53,  2, 28,  85, 89, 170], [14]],
		[[158, 1785, -2009], [214,  86, 118,  13, 137, 28, 85, 149, 90, 238], [13, 15, 19, 22]],
	]

	static CORRECT_NONE = 0;
	static CORRECT_L = 1;
	static CORRECT_M = 2;
	static CORRECT_Q = 3;
	static CORRECT_H = 4;
	static CORRECT_SIZE = 5;

	static CorrectTable = [
		[205, [100, 107, 169, 248, 195], [1, 2]],
		[382, [  1, 175, 243, 255, 255], [20]],
		[565, [ 96, 239, 255, 255, 255], [1, 2, 3, 4]],
		[672, [ 80, 253,  71, 253, 253], []],
	]

	static bchCoding(a, b, n) {
		const s = (n << 1);
		const m = (1 << s);
		let v = (a << n);
		for (let i = 0; i < n; i++) {
			v <<= 1;
			if ((v & m))
				v ^= b;
		}
		return ((a << s) | v);
	}

	static versionBCH(a) {
		return GenQRCodeType.bchCoding(a, 0x1f25, 6);
	}

	static formatBCH(a, micro) {
		const b = (micro ? 0x4445 : 0x5412);
		return GenQRCodeType.bchCoding(a, 0x537, 5) ^ b;
	}

	static FormatBCH = [...Array(4)].map((_, level) =>
		[...Array(8)].map((_, mask) => GenQRCodeType.formatBCH(((level ^ 1) << 3) | mask), false));

	static EncodeTable = [...Array(7)].map(function(_, mode) {
		const U = undefined;
		const micro = (mode < 4);
		const mode_bits  = [0,1,2,3, 4, 4, 4][mode];
		const num_bits   = [3,4,5,6,10,12,14][mode];
		const alnum_bits = [U,3,4,5, 9,11,13][mode];
		const byte_bits  = [U,U,4,5, 8,16,16][mode];
		const kanji_bits = [U,U,3,4, 8,10,12][mode];
		const term_bits  = [3,5,7,9, 4, 4, 4][mode];
		const type_min   = [1,2,3,4, 1,10,27][mode];
		const type_max   = [1,2,3,4, 9,26,40][mode];
		const size_min   = [11,13,15,17, 21, 57,125][mode];
		const size_max   = [11,13,15,17, 53,121,177][mode];
		const lmax = ((b) => (b != U) ? ((1 << b) - 1) : U);
		return {
			mode: mode,
			micro: micro,
			type_min: type_min,
			type_max: type_max,
			size_min: size_min,
			size_max: size_max,

			mode_bits : mode_bits,
			num_bits  : num_bits,
			alnum_bits: alnum_bits,
			byte_bits : byte_bits,
			kanji_bits: kanji_bits,
			term_bits : term_bits,

			num_max  : lmax(num_bits),
			alnum_max: lmax(alnum_bits),
			byte_max : lmax(byte_bits),
			kanji_max: lmax(kanji_bits),

			num_code   : (!micro ? 1 : ((mode >= 1) ? 0 : U)), 
			alnum_code : (!micro ? 2 : ((mode >= 1) ? 1 : U)),
			byte_code  : (!micro ? 4 : ((mode >= 2) ? 2 : U)),
			kanji_code : (!micro ? 8 : ((mode >= 2) ? 3 : U)),
			eci_code   : (!micro ? 7 : U),
			append_code: (!micro ? 3 : U),
			fnc11_code : (!micro ? 5 : U),
			fnc12_code : (!micro ? 9 : U),
		}
	});

	constructor(size) {
		const S = GenQRCodeType;
		this.S = S;

		const st = S.SizeTypeMap.get(size);
		if (st == undefined)
			return undefined;

		const type = st.type;
		const micro = st.micro;

		this.name = ((micro ? 'M' : '') + type);
		this.size = size;
		this.type = type;
		this.micro = micro;
		this.versionBCH = ((type >= 7) ? S.versionBCH(type) : undefined);
		{
			const qmods = (size * size);
			let fmods, tmods;
			if (micro) {
				fmods = 64 + ((size - 8) << 1);
				tmods = 15;
			} else {
				const nalign = (size < 25) ? 0 : Math.trunc((size + 40) / 28);
				const amods = (nalign < 2) ? 0 : ((nalign * nalign - 3) * 25);
				const cmods = (nalign < 3) ? 0 : ((nalign - 2) * 10);
				fmods = 192 + ((size - 16) * 2) + amods - cmods;
				tmods = 31 + ((size < 45) ? 0 : 36);
			}
			const dmods = qmods - fmods - tmods;
			const codes = (dmods + (micro ? 7 : 0)) >> 3;
			const rmods = micro ? 0 : (dmods & 7);

			this.modules = { type: tmods, func: fmods, data: dmods, remainder: rmods };
			this.codeWords = codes;
		}
		this.halfword = ((size == 11) || (size == 15));

		const itype = type - 1;
		const codes = this.codeWords;
		this.correct = [];
		if (size != 11)
			this.correct.push(undefined);
		if (!micro) {
			for (let level = 0; level < 4; level++) {
				const btable = S.BlockTable[level];
				const curv = btable[0];
				const total = (((curv[0] * type * type + curv[1] * type + curv[2]) >> 12)
							   + ((btable[1][itype >> 2] >> ((itype & 3) << 1)) & 3)
							   + ((btable[2].indexOf(type) < 0) ? 0 : 4));

				const count2 = codes % total;
				const size1 = ((codes - count2) / total);
				const count1 = total - count2;
				const size2 = size1 + 1;

				const ctable = S.CorrectTable[level];
				const correct = (((ctable[0] * size1) >> 10)
								 + ((ctable[1][itype >> 3] >> (itype & 7)) & 1)
								 + ((ctable[2].indexOf(type) < 0) ? 0 :
									((level != 2) ? 2 : ((type != 2) ? -1 : -2))));

				const data1 = size1 - correct;
				let words = data1 * count1;
				let bdata = [{ size: size1, count: count1, data: data1, correct: correct }];
				if (count2) {
					const data2 = size2 - correct;
					words += data2 * count2;
					bdata.push({ size: size2, count: count2, data: data2, correct: correct });
				}

				this.correct.push({
					block: bdata,
					data: words,
					data_bits: (words << 3),
					correct: (codes - words),
					format: S.FormatBCH[level],
				});
			}
			this.encodeIndex = (((type >= 10) ? 1 : 0) +
								((type >= 27) ? 1 : 0) + 4);
		} else {
			const crdata = [[2],[5,5],[6,8],[8,10,14]][itype];
			const size = [5,10,17,24][itype];
			const symbol = [0,1,3,5][itype];
			const last_remain = (this.halfword ? 4 : 0);

			for (let level = 0; level < crdata.length; level++) {
				const csize = crdata[level];
				const dsize = codes - csize;
				const bdata = [{ size: size, count: 1, data: dsize, correct: csize }];
				this.correct.push({
					block: bdata,
					data: dsize,
					data_bits: ((dsize << 3) - last_remain),
					correct: csize,
					format: [...Array(4)].map((_, mask) =>
						GenQRCodeType.formatBCH((((level + symbol) << 2) | mask), true)),
				});
			}
			this.encodeIndex = itype;
		}
		this.encode = S.EncodeTable[this.encodeIndex];
	}

	static SizeMap = function() {
		const r = new Map();
		for (let size of GenQRCodeType.SizeListAll)
			r.set(size, new GenQRCodeType(size));
		return r;
	}();

	static fromSize(size) {
		return GenQRCodeType.SizeMap.get(size);
	}

	static fromType(type, micro=false) {
		const S = GenQRCodeType;
		return S.fromSize(S.TypeSizeMap[micro][type]);
	}

	static toUcs2Array(msg) {
		return [...Array(msg.length)].map((_, i) => msg.charCodeAt(i));
	}

	static toUcs4Array(ucs2) {
		const slen = ucs2.length;
		const ucs4 = new Array();
		for (let i = 0; i < slen; i++) {
			const c = ucs2.codePointAt(i);
			ucs4.push(c);
			if (c >= 0x10000)
				i++;
		}
		return ucs4;
	}

	static AlnumMap = {
		48: 0, 49: 1, 50: 2, 51: 3, 52: 4, 53: 5, 54: 6, 55: 7, 56: 8, 57: 9,
		65:10, 66:11, 67:12, 68:13, 69:14, 70:15, 71:16, 72:17, 73:18, 74:19,
		75:20, 76:21, 77:22, 78:23, 79:24, 80:25, 81:26, 82:27, 83:28, 84:29,
		85:30, 86:31, 87:32, 88:33, 89:34, 90:35, 32:36, 36:37, 37:38, 42:39,
		43:40, 45:41, 46:42, 47:43, 58:44,
	}

	static CHAR_NUM    = 0x001;
	static CHAR_ALNUM  = 0x002;
	static CHAR_ASCII  = 0x004;
	static CHAR_BYTE   = 0x008;
	static CHAR_KANJI  = 0x010;
	static CHAR_UTF8   = 0x020;
	static CHAR_CP932  = 0x040;
	static CHAR_CP932L = 0x080;
	static CHAR_CP932H = 0x100;

	static ECI_8859 = 3;
	static ECI_SJIS = 20;
	static ECI_UTF8 = 26;

	static makeCharInfo(c, index) {
		const S = GenQRCodeType;
		const CP932 = GenQRCodeCP932;

		let eci = -1;
		let flag = 0;
		let alnum = S.AlnumMap[c];
		const cp932 = CP932.getCodeEx(c);
		const bit8 = [];
		const utf8 = [];

		if (c < 0x80) {
			flag |= S.CHAR_ASCII | S.CHAR_BYTE | S.CHAR_CP932 | S.CHAR_UTF8;
			bit8.push(c);
			utf8.push(c);
		} else if (c < 0x800) {
			utf8.push(0xc0 | (c >> 6));
			utf8.push(0x80 | (c & 0x3f));
		} else if (c < 0x10000) {
			utf8.push(0xe0 | (c >> 12));
			utf8.push(0x80 | ((c >> 6) & 0x3f));
			utf8.push(0x80 | (c & 0x3f));
		} else {
			utf8.push(0xf0 | (c >> 18));
			utf8.push(0x80 | ((c >> 12) & 0x3f));
			utf8.push(0x80 | ((c >> 6) & 0x3f));
			utf8.push(0x80 | (c & 0x3f));
		}

		if (alnum != undefined) {
			flag |= S.CHAR_ALNUM | S.CHAR_BYTE | S.CHAR_CP932 | S.CHAR_UTF8;
			if (alnum < 10)
				flag |= S.CHAR_NUM;
		} else
			alnum = -1;
		if (cp932 != undefined) {
			flag |= S.CHAR_CP932 | S.CHAR_UTF8;
			const d = cp932.cp932;
			if (cp932.mode == CP932.MODE_KANA) {
				eci = S.ECI_SJIS;
				flag |= S.CHAR_CP932L;
				bit8.push(d);
			} else {
				if (cp932.mode == CP932.MODE_KANJI) {
					flag |= S.CHAR_KANJI;
				} else {
					eci = S.ECI_SJIS;
					flag |= S.CHAR_CP932H;
				}
				bit8.push(d >> 8);
				bit8.push(d & 0xff);
			}
		}
		if (!flag) {
			if (c < 0x100)
				eci = S.ECI_8859;
			else
				eci = S.ECI_UTF8;
			flag |= S.CHAR_UTF8;
		}

		return {
			index: index,
			prev: index - 1,
			next: index + 1,
			count: 1,

			ucs: c,
			eci: eci,
			flag: flag,
			alnum: alnum,
			cp932: cp932,
			bit8: bit8,
			utf8: utf8,
		};
	}

	static makeCharInfoList(msg, ucs4=false) {
		const S = GenQRCodeType;
		const ucs = ucs4 ? S.toUcs4Array(msg) : S.toUcs2Array(msg);
		return [...Array(ucs.length)].map((_, i) => S.makeCharInfo(ucs[i], i));
	}

	encodeMessage(message, jis=false, ucs4=false) {
		const S = GenQRCodeType;
		const charlist = S.makeCharInfoList(message, ucs4);
		const msglen = charlist.length;

		const output = new GenQRCodeBitStream();
		if (!msglen) return output;

		const encmode = this.encode;
		const bs_mode = encmode.mode_bits;

		const bs_num = encmode.num_bits;
		const bs_alnum = encmode.alnum_bits;
		const bs_byte = encmode.byte_bits;
		const bs_kanji = encmode.kanji_bits;

		const bs_m_num = bs_mode + bs_num;
		const bs_m_alnum = bs_mode + bs_alnum;
		const bs_m_byte = bs_mode + bs_byte;
		const bs_m_kanji = bs_mode + bs_kanji;

		const start_eci = (jis ? S.ECI_SJIS : S.ECI_8859);
		let char_flag = 0;
		{
			let prev_flag = 0;
			let last_pos = -1;
			for (let i = 0; i < msglen; i++) {
				const c = charlist[i];
				c.next = i + 1;
				c.count = 1;

				const curr_flag = c.flag;
				const flip_flag = prev_flag ^ curr_flag;

				if (c.alnum >= 0) {
					if (c.alnum < 10)
						char_flag |= S.CHAR_NUM;
					else
						char_flag |= S.CHAR_ALNUM;
				} else {
					char_flag |= S.CHAR_BYTE;
				}

				prev_flag = curr_flag;
				if (!flip_flag)
					continue;
				if (last_pos >= 0) {
					const lc = charlist[last_pos];
					lc.next = i;
					lc.count = i - last_pos;
					c.prev = last_pos;
				}
				last_pos = i;
			}
			if (last_pos >= 0) {
				const lc = charlist[last_pos];
				const count = msglen - last_pos;
				lc.next = msglen;
				lc.count = msglen - last_pos;
			}
		}

		if (encmode.micro &&
			(((bs_alnum == undefined) && (char_flag & S.CHAR_ALNUM)) ||
			 ((bs_byte  == undefined) && (char_flag & S.CHAR_BYTE))))
			return undefined;

		const fscanner = function(callback) {
			let index = 0;
			while ((0 <= index) && (index < msglen)) {
				const curr = charlist[index];
				const prev = ((curr.prev >= 0) ? charlist[curr.prev] : undefined);
				const next = ((curr.next < msglen) ? charlist[curr.next] : undefined);
				index = callback(prev, curr, next);
			}
		};

		const fnum_bits = function(count) {
			const remain = (count % 3);
			const block = (count - remain) / 3;
			const data_bits = (block * 10) + ((remain * 3) + (remain ? 1 : 0));
			return (bs_m_num + data_bits);
		};
		const max_num_bits = fnum_bits(encmode.num_max);

		const falnum_bits = function(count) {
			const remain = count & 1;
			const block = (count - remain) >> 1;
			const data_bits = (block * 11) + (remain * 6);
			return (bs_m_alnum + data_bits);
		};
		const max_alnum_bits = fnum_bits(encmode.alnum_max);

		const fbyte_bits = ((count) => bs_m_byte + (count * 8));
		const max_byte_bits = fbyte_bits(encmode.byte_max);

		const fkanji_bits = ((count) => bs_m_kanji + (count * 13));
		const max_kanji_bits = fbyte_bits(encmode.kanji_max);

		const fsegment_bits = function(count, segsize, segbits, fbits) {
			const remain = count % segsize;
			const segment = (count - remain) / segsize;
			return (segment * segbits + (remain ? fbits(remain) : 0));
		};
		const fsegment_bits_sc = ((start, end, segsize, segbits, fbits) => fsegment_bits(end - start, segsize, segbits, fbits));
		const fsegment_bits_mc = ((start, end, segsize, segbits, fbits, fscan) => fsegment_bits(fscan(start, end), segsize, segbits, fbits));

		const fscan_byte = ((start, end) => charlist.slice(start, end).reduce(((p, c) => p + c.bit8.length), 0));
		const fscan_utf8 = ((start, end) => charlist.slice(start, end).reduce(((p, c) => p + c.utf8.length), 0));

		const fseg_num_bits   = ((start, end) => fsegment_bits_sc(start, end, encmode.num_max, max_num_bits, fnum_bits));
		const fseg_alnum_bits = ((start, end) => fsegment_bits_sc(start, end, encmode.alnum_max, max_alnum_bits, falnum_bits));
		const fseg_kanji_bits = ((start, end) => fsegment_bits_sc(start, end, encmode.kanji_max, max_kanji_bits, fkanji_bits));
		const fseg_byte_bits  = ((start, end) => fsegment_bits_mc(start, end, encmode.byte_max, max_byte_bits, fbyte_bits, fscan_byte));
		const fseg_utf8_bits  = ((start, end) => fsegment_bits_mc(start, end, encmode.byte_max, max_byte_bits, fbyte_bits, fscan_utf8));

		const fseg_auto_bits = ((flag, start, end) =>
			((flag & S.CHAR_NUM) ? fseg_num_bits(start, end)
			 : (flag & S.CHAR_ALNUM) ? fseg_alnum_bits(start, end)
			 : (flag & S.CHAR_KANJI) ? fseg_kanji_bits(start, end)
			 : (flag & (S.CHAR_BYTE | S.CHAR_CP932)) ? fseg_byte_bits(start, end)
			 : (flag & S.CHAR_UTF8) ? fseg_utf8_bits(start, end) : 0x10000));

		const feci_update2 = ((a, b) => ((a == b) ? a :
										 (a == -1) ? b :
										 (b == -1) ? a :
										 (a == S.ECI_UTF8) ? a :
										 (b == S.ECI_UTF8) ? b : a));
		const feci_update3 = ((a, b, c) => feci_update2(feci_update2(a, b), c));

		const fmerge_check = function(curr, other, omask) {
			if (other == undefined) return false;
			if (!(other.flag & omask)) return false;
			if (curr.eci == other.eci) return true;
			if ((curr.eci == -1) || (other.eci == -1)) return true;
			if (((curr.eci == S.ECI_8859) || (curr.eci == S.ECI_SJIS)) && (other.eci == S.ECI_UTF8)) return true;
			if ((curr.eci == S.ECI_UTF8) && ((other.eci == S.ECI_8859) || (other.eci == S.ECI_SJIS))) return true;
			return false;
		};

		let merged = false;
		const fmerger = function(cmask, omask) {
			fscanner(function(prev, curr, next) {
				if (!(curr.flag & (cmask | omask)))
					return curr.next;

				const pmerge = fmerge_check(curr, prev, omask);
				const nmerge = fmerge_check(curr, next, omask);
				const pbits = (pmerge) ? fseg_auto_bits(prev.flag, prev.index, prev.next) : 0;
				const cbits = fseg_auto_bits(curr.flag, curr.index, curr.next);
				const nbits = (nmerge) ? fseg_auto_bits(next.flag, next.index, next.next) : 0;

				if (pmerge && nmerge && fmerge_check(prev, next, omask)) {
					const slen = pbits + cbits + nbits;
					const mlen = fseg_auto_bits(omask, prev.index, next.next);
					if (slen > mlen) {
						prev.flag &= (curr.flag & next.flag);
						prev.eci = feci_update3(prev.eci, curr.eci, next.eci);
						prev.next = next.next;
						prev.count = (prev.next - prev.index);
						if (prev.next < msglen)
							charlist[prev.next].prev = prev.index;
						merged = true;
						return prev.index;
					}
				}
				if (pmerge) {
					const slen = pbits + cbits;
					const mlen = fseg_auto_bits(omask, prev.index, curr.next);
					if (slen <= mlen) return curr.next;

					prev.flag &= curr.flag;
					prev.eci = feci_update2(prev.eci, curr.eci);
					prev.next = curr.next;
					prev.count = (prev.next - prev.index);
					if (next != undefined)
						next.prev = prev.index;
					merged = true;
					return prev.index;
				}
				if (nmerge) {
					const slen = cbits + nbits;
					const mlen = fseg_auto_bits(omask, curr.index, next.next);
					if (slen <= mlen) return curr.next;

					curr.flag &= next.flag;
					curr.eci = feci_update2(curr.eci, next.eci);
					curr.next = next.next;
					curr.count = (curr.next - curr.index);
					if (curr.next < msglen)
						charlist[curr.next].prev = curr.index;
					merged = true;
					return curr.index;
				}
				return curr.next;
			});
		};

		do {
			merged = false;
			fmerger(S.CHAR_NUM, S.CHAR_ALNUM);
			fmerger(S.CHAR_NUM, S.CHAR_BYTE);
			fmerger(S.CHAR_NUM, S.CHAR_CP932);
			fmerger(S.CHAR_NUM, S.CHAR_UTF8);
			fmerger(S.CHAR_ALNUM, S.CHAR_BYTE);
			fmerger(S.CHAR_ALNUM, S.CHAR_CP932);
			fmerger(S.CHAR_ALNUM, S.CHAR_UTF8);
			fmerger(S.CHAR_KANJI, S.CHAR_CP932);
			fmerger(S.CHAR_KANJI, S.CHAR_UTF8);
			fmerger(S.CHAR_ASCII, S.CHAR_BYTE);
			fmerger(S.CHAR_BYTE, S.CHAR_CP932);
			fmerger(S.CHAR_CP932, S.CHAR_UTF8);
		} while (merged);

		const fout_num = function(span) {
			let index = span.index;
			let count = span.count;
			while (count > 0) {
				let seglen = count;
				if (seglen >= encmode.num_max)
					seglen = encmode.num_max;
				count -= seglen;

				output.write(encmode.num_code, bs_mode);
				output.write(seglen, bs_num);
				while (seglen >= 3) {
					const c1 = charlist[index++].alnum;
					const c2 = charlist[index++].alnum;
					const c3 = charlist[index++].alnum;
					output.write((c1 * 100 + c2 * 10 + c3), 10);
					seglen -= 3;
				}
				if (seglen == 2) {
					const c1 = charlist[index++].alnum;
					const c2 = charlist[index++].alnum;
					output.write((c1 * 10 + c2), 7);
				} else if (seglen == 1)
					output.write(charlist[index++].alnum, 4);
			}
		};

		const fout_alnum = function(span) {
			let index = span.index;
			let count = span.count;
			while (count > 0) {
				let seglen = count;
				if (seglen >= encmode.alnum_max)
					seglen = encmode.alnum_max;
				count -= seglen;

				output.write(encmode.alnum_code, bs_mode);
				output.write(seglen, bs_alnum);
				while (seglen >= 2) {
					const c1 = charlist[index++].alnum;
					const c2 = charlist[index++].alnum;
					output.write((c1 * 45 + c2), 11);
					seglen -= 2;
				}
				if (seglen == 1)
					output.write(charlist[index++].alnum, 6);
			}
		};

		const fout_byte_data = function(byte_data) {
			let index = 0;
			let count = byte_data.length;
			while (count > 0) {
				let seglen = count;
				if (seglen >= encmode.byte_max)
					seglen = encmode.byte_max;
				count -= seglen;

				output.write(encmode.byte_code, bs_mode);
				output.write(seglen, bs_byte);
				while (seglen-- > 0)
					output.write(byte_data[index++], 8);
			}
		};
		const fout_byte = function(span) {
			let sp = span.index;
			let ep = sp + span.count;
			const byte_data = [];
			while (sp < ep)
				for (let b of charlist[sp++].bit8)
					byte_data.push(b);
			fout_byte_data(byte_data);
		};
		const fout_utf8 = function(span) {
			let sp = span.index;
			let ep = sp + span.count;
			const byte_data = [];
			while (sp < ep)
				for (let b of charlist[sp++].utf8)
					byte_data.push(b);
			fout_byte_data(byte_data);
		};

		const fout_kanji = function(span) {
			let index = span.index;
			let count = span.count;
			while (count > 0) {
				let seglen = count;
				if (seglen >= encmode.kanji_max)
					seglen = encmode.kanji_max;
				count -= seglen;

				output.write(encmode.kanji_code, bs_mode);
				output.write(seglen, bs_kanji);
				while (seglen-- > 0)
					output.write(charlist[index++].cp932.kanji, 13);
			}
		};

		const feci_neq = ((a, b) => (a >= 0) && (b >= 0) && (a != b));
		{
			let curr_eci = start_eci;
			fscanner(function(prev, curr, next) {
				const flag = curr.flag;
				curr.eci = -1;
				if (!(flag & (S.CHAR_NUM | S.CHAR_ALNUM | S.CHAR_ASCII | S.CHAR_KANJI))) {
					if ((flag & S.CHAR_CP932))
						curr.eci = S.ECI_SJIS;
					else if ((flag & S.CHAR_UTF8))
						curr.eci = S.ECI_UTF8;
				}
				return curr.next;
			});
		}
		{
			let use_eci = false;
			let curr_eci = start_eci;
			fscanner(function(p_, curr, n_) {
				const eci = curr.eci;
				const next = curr.next;
				if (feci_neq(curr_eci, eci)) {
					curr_eci = eci;
					output.write(encmode.eci_code, bs_mode);
					if (eci < 0x80) {
						output.write(eci, 8);
					} else if (eci < 0x4000) {
						output.write(((eci >> 8) | 0x80), 8);
						output.write((eci & 0xff), 8);
					} else {
						output.write(((eci >> 16) | 0xc0), 8);
						output.write(((eci >> 8) & 0xff), 8);
						output.write((eci & 0xff), 8);
					}
					use_eci = true;
				}
				const flag = curr.flag;
				if ((flag & S.CHAR_NUM)) { fout_num(curr); return next; }
				if ((flag & S.CHAR_ALNUM)) { fout_alnum(curr); return next; }
				if ((flag & S.CHAR_KANJI)) { fout_kanji(curr); return next; }
				if ((curr_eci == S.ECI_UTF8) &&
					((flag & (S.CHAR_CP932 | S.CHAR_UTF8)))) {
					fout_utf8(curr);
				} else
					fout_byte(curr);
				return next;
			});
			if (use_eci && encmode.micro)
				return undefined;
		}
		return output;
	}

	checkCodingSize(level, data_bits) {
		const correct = this.correct[level];
		return ((correct == undefined) ? false :
				(data_bits <= correct.data_bits));
	}

	static autoEncodeMessage(message, option=undefined) {
		const S = GenQRCodeType;
		const jis = (option ?? {}).jis ?? false;
		const sizelist = (option ?? {}).size ?? S.SizeListAll;
		const output = new Array(S.CORRECT_SIZE);
		let encid = -1;
		let coding = undefined;
		let find = -1;
		for (let size of sizelist) {
			const qr = S.fromSize(size);
			if (encid != qr.encodeIndex) {
				encid = qr.encodeIndex;
				coding = qr.encodeMessage(message, jis, true);
			}
			if (coding == undefined)
				continue;

			let coding_size = coding.bit_length;
			const correct = qr.correct;
			for (let level = 0; level < S.CORRECT_SIZE; level++) {
				if (output[level] != undefined) continue;
				if (qr.checkCodingSize(level, coding_size)) {
					output[level] = { type: qr, level: level, data: coding };
					find = level + 1;
				}
			}
			if (find == S.CORRECT_SIZE)
				break;
		}
		return output;
	}

	createMessageCode(correct_level, qrcode) {
		const P = GenQRCodeRSGFB8Polynomial;
		const correct = this.correct[correct_level];

		const term_bits = this.encode.term_bits;
		let data_remain = correct.data_bits - qrcode.bit_length;
		if (data_remain < 0) return undefined;
		const term_length = ((data_remain >= term_bits) ? term_bits : data_remain);

		const newcode = new GenQRCodeBitStream(qrcode);
		newcode.write(0, term_length);
		data_remain -= term_length;
		let ncrem = (newcode.bit_length & 7);
		if (ncrem > 0) {
			newcode.write(0, ncrem);
			data_remain -= ncrem;
		}
		while (data_remain >= 16) {
			newcode.write(0xec, 8);
			newcode.write(0x11, 8);
			data_remain -= 16;
		}
		if (data_remain >= 8) {
			newcode.write(0xec, 8);
			data_remain -= 8;
		}
		if (data_remain > 0) {
			newcode.write(data_remain, 8);
			data_remain = 0;
		}

		const msgblk = [];
		const crblk = [];
		let dpos = 0;
		let dvirtlen = 0;
		let cvirtlen = 0;
		for (let block of correct.block) {
			let dsize = block.data;
			let csize = block.correct;
			if (dvirtlen < dsize) dvirtlen = dsize;
			if (cvirtlen < csize) cvirtlen = csize;
			const dadd = Array(csize).fill(0);
			const pgen = P.fromGenerator(csize);
			for (let bcnt = block.count; bcnt > 0; bcnt--) {
				let dend = dpos + dsize;
				const dblk = newcode.slice(dpos, dend);
				msgblk.push(dblk);
				const rsc = P.fromData(dblk.concat(dadd), true).mod(pgen).polynomial;
				while (rsc.length < csize) rsc.push(0);
				crblk.push([...Array(rsc.length)].map((_, i) => rsc[rsc.length - i - 1].element));
				dpos = dend;
			}
		}

		const r_data = [];
		for (let i = 0; i < dvirtlen; i++)
			for (let d of msgblk)
				if (i < d.length)
					r_data.push(d[i]);

		const r_correct = [];
		for (let i = 0; i < cvirtlen; i++)
			for (let c of crblk)
				if (i < c.length)
					r_correct.push(c[i]);

		let r_codeword;
		if (this.micro && ((this.type == 1) || (this.type == 3))) {
			let lcw = r_data[r_data.length - 1];
			r_codeword = r_data.slice(0, r_data.length - 1);
			for (let d of r_correct) {
				r_codeword.push(lcw | (d >> 4));
				lcw = (d & 15) << 4;
			}
			r_codeword.push(lcw);
		} else
			r_codeword = r_data.concat(r_correct);
		r_codeword.push(0);

		return { codeword: r_codeword, data: r_data, correct: r_correct };
	}

	decodeMessage(qrcode, jis=false) {
		const S = GenQRCodeType;
		const encmode = this.encode;

		const data = new GenQRCodeBitStream(qrcode);
		const ALNUM = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';
		const eci3dec = new TextDecoder('iso8859-1');
		const sjisdec = new TextDecoder('shift_jis');
		const utf8dec = new TextDecoder();

		let eci = (jis ? S.ECI_SJIS : S.ECI_8859);
		let msg = '';
		for (;;) {
			const mode = data.read(encmode.mode_bits);
			switch (mode) {
			case undefined:
			case 0:
				return msg;
			case encmode.num_code:
				{
					let len = data.read(encmode.num_bits);
					while (len >= 3) {
						const d1 = data.read(10);
						const c1 = d1 % 10;
						const d2 = ((d1 - c1) / 10);
						const c2 = d2 % 10;
						const c3 = ((d2 - c2) / 10);
						msg += c3;
						msg += c2;
						msg += c1;
						len -= 3;
					}
					switch (len) {
					case 2:
						{
							const d1 = data.read(7);
							const c1 = d1 % 10;
							const c2 = ((d1 - c1) / 10);
							msg += c2;
							msg += c1;
						}
						break;
					case 1:
						msg += data.read(4);
						break;
					}
				}
				break;
			case encmode.alnum_code:
				{
					let len = data.read(encmode.alnum_bits);
					while (len >= 2) {
						const d1 = data.read(11);
						const c1 = d1 % 45;
						const c2 = (d1 - c1) / 45;
						msg += ALNUM[c2];
						msg += ALNUM[c1];
						len -= 2;
					}
					if (len == 1)
						msg += ALNUM[data.read(6)];
				}
				break;
			case encmode.kanji_code:
				{
					const len = data.read(encmode.kanji_bits);
					let kanji = [];
					while (kanji.length < (len << 1)) {
						const d = data.read(13);
						let l = (d % 192);
						let h = (d - l) / 192;
						kanji.push(h + ((h < 0x20) ? 0x81 : 0xc1));
						kanji.push(l + 0x40);
					}
					msg += sjisdec.decode(new Uint8Array(kanji));
				}
				break;
			case encmode.byte_code:
				{
					const len = data.read(encmode.byte_bits);
					const bytes = new Uint8Array([...Array(len)].map(() => data.read(8)));
					switch (eci) {
					case S.ECI_8859: msg += eci3dec.decode(bytes); break;
					case S.ECI_SJIS: msg += sjisdec.decode(bytes); break;
					case S.ECI_UTF8: msg += utf8dec.decode(bytes); break;
					default: return undefined;
					}
				}
				break;
			case encmode.eci_code:
				{
					const b1 = data.read(8);
					if (b1 < 0x80) {
						eci = b1;
						break;
					}
					const b2 = data.read(8);
					if (b1 < 0xc0) {
						eci = ((b1 & 0x3f) << 8) | b2;
						break;
					}
					const b3 = data.read(8);
					eci = (((b1 & 0x1f) << 16) | (b2 << 8) | b3);
				}
				break;
			default:
				return undefined;
			}
		}
	}

	static escapeBackslash(message) { return message.replace('\\', '\\\\'); }
	static unescapeBackslash(message) { return message.replace('\\\\', '\\'); }
}

class GenQRCodeRSGFB8 {
	static ELEMENT_TABLE = function() {
		let a = 1;
		return [...Array(255)].map((_, i) =>
			!i ? a : (((a *= 2) < 256) ? a : (a ^= 285)));
	}();

	static EXPONENT_TABLE = function() {
		const e = GenQRCodeRSGFB8.ELEMENT_TABLE;
		return [...Array(256)].map((_, i) =>
			!i ? undefined : e.indexOf(i));
	}();

	static fromElement(element) {
		return new GenQRCodeRSGFB8(element, GenQRCodeRSGFB8.EXPONENT_TABLE[element]);
	}

	static fromExponent(exponent) {
		return new GenQRCodeRSGFB8(GenQRCodeRSGFB8.ELEMENT_TABLE[exponent], exponent);
	}

	constructor(element, exponent) {
		this.element = element;
		this.exponent = exponent;
	}

	isZero() { return (!this.element); }

	addElement(rhs) { return GenQRCodeRSGFB8.fromElement(this.element ^ rhs.element); }

	addExponent(value) {
		return (this.element
				? GenQRCodeRSGFB8.fromExponent((this.exponent + value) % 255)
				: GenQRCodeRSGFB8.fromElement(0));
	}
}

class GenQRCodeRSGFB8Polynomial {
	static GENERATOR_POLYNOMIAL = function() {
		const S = GenQRCodeRSGFB8;
		const p = S.ELEMENT_TABLE;
		const q = S.EXPONENT_TABLE;
		const z = S.fromExponent(0);
		let g = [z, z];
		return [...Array(69)].map((_, i) =>
			((i == 0) ? [] :
			 ((i == 1) ? g :
			  (g = [...Array(i+1)].map((_, j) =>
				  ((j == 0) ? g[0].addExponent(i - 1) :
				   ((j == i) ? z : g[j - 1].addElement(g[j].addExponent(i - 1)))))))));
	}();

	static fromGenerator(level) {
		const S = GenQRCodeRSGFB8Polynomial;
		return new S(S.GENERATOR_POLYNOMIAL[level]);
	}

	static fromData(data, reverse=false) {
		const dlen = data.length - 1;
		return new GenQRCodeRSGFB8Polynomial(
			[...Array(data.length)].map((_, i) =>
				GenQRCodeRSGFB8.fromElement(data[reverse ? (dlen - i) : i])));
	}

	constructor(polynomial) {
		this.polynomial = polynomial;
	}

	validLength() {
		const p = this.polynomial;
		for (let n = p.length; n > 0; n--)
			if (!p[n - 1].isZero())
				return n;
		return 0;
	}

	mod(rhs) {
		const rplen = rhs.validLength();
		if (!rplen) return undefined;

		const org = this.polynomial;
		let lplen = this.validLength();
		const lp = [...Array(lplen)].map((_, i) => org[i]);

		const rp = rhs.polynomial;
		const rptop = rp[rplen - 1];
		const rpexp = 255 - rptop.exponent;

		for (let roffs = lplen - rplen; roffs >= 0; roffs--, lplen--) {
			const lptop = lp[lplen - 1];
			if (lptop.isZero()) continue;
			const lpexp = lptop.exponent;
			const mexp = (lpexp + rpexp) % 255;
			for (let p = roffs, q = 0; p < lplen; p++, q++)
				lp[p] = lp[p].addElement(rp[q].addExponent(mexp));
		}
		return new GenQRCodeRSGFB8Polynomial(lp.slice(0, lplen));
	}
}

class GenQRCodeBitStream {
	constructor(src=undefined) {
		const self = this;
		if (src != undefined) {
			if (src.data != undefined) {
				this.data = [].concat(src.data);
				this.rpos = src.rpos;
				this.wpos = src.wpos;
			} else {
				this.data = src;
				this.rpos = 0;
				this.wpos = (src.length << 3);
			}
		} else {
			this.data = [];
			this.rpos = 0;
			this.wpos = 0;
		}
		return new Proxy(this, {
			get: ((target, prop) =>
				(prop in target) ? target[prop] : self.data[prop]
			),
		});
	}

	slice(start, end) {
		return this.data.slice(start, end);
	}

	clone() { return GenQRCodeBitStream(this); }

	read(bits) {
		let pos = this.rpos;
		let rem = this.wpos - pos;
		if (rem > bits) rem = bits;
		this.rpos = pos + rem;

		const data = this.data;
		let value = 0;
		while (rem > 0) {
			let rbits = (8 - (pos & 7));
			let step = ((rbits < rem) ? rbits : rem);
			value = ((value << step) |
					 ((data[pos >> 3] >> (rbits - step)) &
					  ((1 << step) - 1)));
			pos += step;
			rem -= step;
		}
		return value;
	}

	write(data, bits) {
		if (bits <= 0)
			return;

		let pos = this.wpos;
		let rbits = (pos & 7);
		const buffer = this.data;
		this.wpos = pos + bits;

		if (rbits) {
			rbits = 8 - rbits;

			const last = buffer.length - 1;
			const mask = (1 << rbits) - 1;
			if (rbits >= bits) {
				buffer[last] |= (data << (rbits - bits)) & mask;
				return;
			}
			buffer[last] |= (data >> ((bits - rbits))) & mask;
			bits -= rbits;
		}
		while (bits > 8) {
			bits -= 8;
			buffer.push((data >> bits) & 0xff);
		}
		buffer.push((data << (8 - bits)) & 0xff);
	}

	[Symbol.iterator]() { return this.data[Symbol.iterator](); }
	get length() { return this.data.length; }
	get bit_length() { return this.wpos; }
}

class GenQRCodeMatrix {
	static FinderPattern = [...Array(7)].map((_, i) => [...Array(7)].map(
		(_, j) => (Math.max(Math.abs(i - 3), Math.abs(j - 3)) == 2) ? 2 : 1));

	static AlignmentPattern = [...Array(5)].map((_, i) => [...Array(5)].map(
		(_, j) => (Math.max(Math.abs(i - 2), Math.abs(j - 2)) == 1) ? 2 : 1));

	static TimingPattern = [...Array(177)].map((_, i) => ((i & 1) + 1));

	constructor(type) {
		const S = GenQRCodeMatrix;

		this.S = S;
		this.FinderPattern = S.FinderPattern;
		this.AlignmentPattern = S.AlignmentPattern;
		this.TimingPattern = S.TimingPattern;

		this.type = type;
		this.micro = type.micro;
		this.module_size = type.size;

		this.clearMatrix();
		this.setFunctionPattern();
	}

	clearMatrix() {
		const size = this.module_size;
		this.matrix = [...Array(size)].map(() => Array(size).fill(0));
	}

	clone(mask=3) {
		const robj = new GenQRCodeMatrix(this.type);
		const size = this.module_size;

		for (let y = 0; y < size; y++) {
			const sl = this.matrix[y];
			const dl = robj.matrix[y];
			for (let x = 0; x < size; x++)
				dl[x] = sl[x] & mask;
		}
		return robj;
	}

	getValue(x, y) { return this.matrix[y][x]; }
	setValue(x, y, value) { this.matrix[y][x] = value; }

	setPattern(x, y, pattern) {
		const matrix = this.matrix;
		for (let span of pattern) {
			const line = matrix[y];
			matrix[y++] = line.slice(0, x).concat(
				span,
				line.slice(x + span.length)
			);
		}
	}

	setXLine(value, y, x1, x2) {
		if (x1 > x2)
			[x1, x2] = [x2, x1];
		x2++;

		const matrix = this.matrix;
		const line = matrix[y];
		matrix[y] = line.slice(0, x1).concat(
			Array(x2 - x1).fill(value),
			line.slice(x2)
		);
	}

	setYLine(value, x, y1, y2) {
		if (y1 > y2)
			[y1, y2] = [y2, y1];

		const matrix = this.matrix;
		for (let y = y1; y <= y2; y++)
			matrix[y][x] = value;
	}

	setFinder() {
		const pattern = this.FinderPattern;
		const z0 = this.module_size - 7;
		const z6 = z0 + 6;
		const za = z0 - 1;

		this.setPattern(z0, z0, pattern);
		this.setXLine(2, za, za, z6);
		this.setYLine(2, za, z0, z6);
		if (this.micro) return;

		this.setPattern(z0, 0, pattern);
		this.setXLine(2,  7, za, z6);
		this.setYLine(2, za,  0,  6);

		this.setPattern(0, z0, pattern);
		this.setXLine(2, za,  0,  7);
		this.setYLine(2,  7, z0, z6);
	}

	setAlignment() {
		const size = this.module_size;
		if (size < 25) return;

		const plast = size - 9;
		const steps = Math.trunc((size + 12) / 28);
		const ptmp = ((((size - 13) + (steps >> 1)) / steps) + 1) & ~1;
		const pstep = ptmp + (ptmp & 1);
		const pos = [...Array(steps)].map((_, i) => (i * pstep) + 4);
		pos.push(plast);

		const pattern = this.AlignmentPattern;
		const pcnt = pos.length;

		const qcnt = pcnt - 1;
		for (let x = 0; x < qcnt; x++)
			this.setPattern(pos[x], 4, pattern);
		for (let y = 1; y < qcnt; y++) {
			const ay = pos[y];
			for (let x = 0; x < pcnt; x++)
				this.setPattern(pos[x], ay, pattern);
		}
		for (let x = 1; x < qcnt; x++)
			this.setPattern(pos[x], plast, pattern);
	}

	setTiming() {
		const pattern = this.TimingPattern;
		const matrix = this.matrix;
		const size = this.module_size;
		const last = size - 1;
		const term = size - 7;
		let x = term;
		let ys = 8;
		let ye = term;

		if (!this.micro) {
			let line = matrix[ye];
			matrix[ye] = line.slice(0, 8).concat(
				pattern.slice(8, term),
				line.slice(term)
			);
		} else {
			let line = matrix[last];
			matrix[last] = pattern.slice(0, term).concat(
				line.slice(term, size)
			);
			x = last;
			ys = 0;
		}

		let value = 1;
		for (let y = ys; y < ye; y++) {
			matrix[y][x] = value;
			value = 3 - value;
		}
	}

	setFunctionPattern() {
		this.setFinder();
		this.setTiming();
		this.setAlignment();
	}

	setFormat(format=0, version=0) {
		const matrix = this.matrix;
		const size = this.module_size;
		const last = size - 1;
		const last1 = last - 1;
		const last6 = last - 6;
		const last8 = last - 8;

		if (this.micro) {
			let bl = 0;
			let bh = 14;
			let x = last1;
			let y = last8;
			let line = matrix[y];
			for (let i = 0; i < 8; i++) {
				line[x]      = (2 - ((format >> bh--) & 1));
				matrix[x][y] = (2 - ((format >> bl++) & 1));
				x--;
			}
			return; /* ** */
		} else {
			let bl = 0;
			let bh = 14;
			let x;// = 0;
			let y = last8;
			let line = matrix[y];
			for (x = 0; x < 7; x++) {
				line[x]      = (2 - ((format >> bh--) & 1));
				matrix[x][y] = (2 - ((format >> bl++) & 1));
			}
			line[x] = 1;
			matrix[x][y] = (2 - ((format >> bl++) & 1));
			x = last8;
			matrix[x][y] = (2 - ((format >> bh--) & 1));
			x++;
			line[x]      = (2 - ((format >> bh--) & 1));
			matrix[x][y] = (2 - ((format >> bl++) & 1));
			x++;
			while (++x < size) {
				line[x]      = (2 - ((format >> bh--) & 1));
				matrix[x][y] = (2 - ((format >> bl++) & 1));
			}
		}
		if (size >= 45) {
			let x = last;
			let y = 8;
			let line3 = matrix[ 8];
			let line2 = matrix[ 9];
			let line1 = matrix[10];
			for (let i = 0; i < 6; i++) {
				let v1 = (2 - (version & 1)); version >>= 1;
				let v2 = (2 - (version & 1)); version >>= 1;
				let v3 = (2 - (version & 1)); version >>= 1;
				line1[x] = v1;
				line2[x] = v2;
				line3[x] = v3;
				matrix[x][y]     = v3;
				matrix[x][y + 1] = v2;
				matrix[x][y + 2] = v1;
				x--;
			}
		}
	}

	createImage(codeword, pattern_id) {
		const S = this.S;
		const micro = this.micro;
		const matrix = this.matrix;
		const size = this.module_size;
		const last = size - 1;
		const skip = (micro ? -1 : size - 7);

		const pattern = S.maskPattern[micro][pattern_id];
		const mask_offset = (180 - size) % 12; // 12*15 = 177+3

		const image = this.clone(1);
		const imgmtx = image.matrix;
		const color = ((p, x, y) => (((codeword[p >> 3] >> (7 ^ (p & 7))) & 1)
									 ^ pattern[(mask_offset + y) % 12][(mask_offset + x) % 12]));

		let cwp = 0;
		for (let y1 = 0; y1 < last; y1 += 2) {
			{
				const y2 = y1 + 1;
				const srow1 = matrix[y1];
				const srow2 = matrix[y2];
				const drow1 = imgmtx[y1];
				const drow2 = imgmtx[y2];

				for (let x = 0; x < size; x++) {
					if (!srow1[x]) drow1[x] = color(cwp++, x, y1);
					if (!srow2[x]) drow2[x] = color(cwp++, x, y2);
				}
			}
			y1 += 2;
			if (y1 == skip)
				y1++;
			else if (y1 >= last)
				break;
			{
				const y2 = y1 + 1;
				const srow1 = matrix[y1];
				const srow2 = matrix[y2];
				const drow1 = imgmtx[y1];
				const drow2 = imgmtx[y2];

				for (let x = size - 1; x >= 0; x--) {
					if (!srow1[x]) drow1[x] = color(cwp++, x, y1);
					if (!srow2[x]) drow2[x] = color(cwp++, x, y2);
				}
			}
		}

		return image;
	}

	createAllImage(correct_level, codedata) {
		const qrtype = this.type;
		const msgcode = qrtype.createMessageCode(correct_level, codedata);
		const codeword = msgcode.codeword;
		const correct = qrtype.correct[correct_level];
		const format = correct.format;
		const fmtlen = format.length;
		const versionBCH = qrtype.versionBCH;
		const image = [];
		for (let i = 0; i < format.length; i++) {
			this.setFormat(format[i], versionBCH);
			image.push(this.createImage(codeword, i));
		}
		return image;
	}

	getPatternScore() {
		const qrtype = this.type;
		const size = this.module_size;
		const last = size - 1;

		if (qrtype.micro) {
			let sum1 = 0;
			let sum2 = 0;
			for (let p = 0; p < last; p++) {
				sum1 += this.getValue(p, 0);
				sum2 += this.getValue(0, p);
			}
			if (sum1 > sum2)
				[sum1, sum2] = [sum2, sum1];
			return (sum1 * 16 + sum2);
		}

		const matrix = this.matrix;
		let score = 0;
		{
			for (let y = 0; y < size; y++) {
				let last = -1;
				let count = 0;
				for (let v of matrix[y]) {
					if (v == last) {
						count++;
						continue;
					}
					if (count >= 5)
						score += (count - 2);
					last = v;
					count = 0;
				}
				if (count >= 5)
					score += (count - 2);
			}
			for (let x = 0; x < size; x++) {
				let last = -1;
				let count = 0;
				for (let y = 0; y < size; y++) {
					const v = matrix[y][x];
					if (v == last) {
						count++;
						continue;
					}
					if (count >= 5)
						score += (count - 2);
					last = v;
					count = 0;
				}
				if (count >= 5)
					score += (count - 2);
			}
		}
		{
			for (let y = 0; y < last; y++) {
				const line1 = matrix[y];
				const line2 = matrix[y + 1];

				for (let x = 0; x < last; x++) {
					const v11 = line1[x];
					const v12 = line1[x + 1];
					const v21 = line2[x];
					const v22 = line2[x + 1];

					if ((v11 == v12) && (v11 == v21) && (v11 == v22))
						score += 3;
				}
			}
		}
		{
			const w = size - 11;

			for (let y = 0; y < size; y++) {
				const line = matrix[y];
				for (let x = 0; x < w; x++) {
					do {
						if (line[x +  0] != 1) break;
						if (line[x +  1] != 0) break;
						if (line[x +  2] != 1) break;
						if (line[x +  3] != 1) break;
						if (line[x +  4] != 1) break;
						if (line[x +  5] != 0) break;
						if (line[x +  6] != 1) break;
						if (line[x +  7] != 0) break;
						if (line[x +  8] != 0) break;
						if (line[x +  9] != 0) break;
						if (line[x + 10] != 0) break;
						score += 40;
					} while (false);
					do {
						if (line[x +  0] != 0) break;
						if (line[x +  1] != 0) break;
						if (line[x +  2] != 0) break;
						if (line[x +  3] != 0) break;
						if (line[x +  4] != 1) break;
						if (line[x +  5] != 0) break;
						if (line[x +  6] != 1) break;
						if (line[x +  7] != 1) break;
						if (line[x +  8] != 1) break;
						if (line[x +  9] != 0) break;
						if (line[x + 10] != 1) break;
						score += 40;
					} while (false);
				}
			}

			for (let x = 0; x < size; x++) {
				for (let y = 0; y < w; y++) {
					do {
						if (matrix[y +  0][x] != 1) break;
						if (matrix[y +  1][x] != 0) break;
						if (matrix[y +  2][x] != 1) break;
						if (matrix[y +  3][x] != 1) break;
						if (matrix[y +  4][x] != 1) break;
						if (matrix[y +  5][x] != 0) break;
						if (matrix[y +  6][x] != 1) break;
						if (matrix[y +  7][x] != 0) break;
						if (matrix[y +  8][x] != 0) break;
						if (matrix[y +  9][x] != 0) break;
						if (matrix[y + 10][x] != 0) break;
						score += 40;
					} while (false);
					do {
						if (matrix[y +  0][x] != 0) break;
						if (matrix[y +  1][x] != 0) break;
						if (matrix[y +  2][x] != 0) break;
						if (matrix[y +  3][x] != 0) break;
						if (matrix[y +  4][x] != 1) break;
						if (matrix[y +  5][x] != 0) break;
						if (matrix[y +  6][x] != 1) break;
						if (matrix[y +  7][x] != 1) break;
						if (matrix[y +  8][x] != 1) break;
						if (matrix[y +  9][x] != 0) break;
						if (matrix[y + 10][x] != 1) break;
						score += 40;
					} while (false);
				}
			}
		}
		{
			const dark = matrix.reduce(((yp, line) => yp + line.reduce(((xp, v) => xp + v), 0)), 0);
			const rate = Math.trunc((dark * 100) / (size * size));
			score += 10 * Math.trunc(Math.abs(rate - 50) / 5);
		}
		return score;
	}

	createBestImage(correct_level, codedata) {
		const images = this.createAllImage(correct_level, codedata);
		const scores = images.map(img => img.getPatternScore());
		return images[scores.indexOf(Math.min.apply(null, scores))];
	}

	static makeMaskPattern = ((f) => [...Array(12)].map((_, j) => [...Array(12)].map((_, i) => f(11-i, 11-j))));
	static maskPattern0 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!((i + j) & 1) ? 1 : 0));
	static maskPattern1 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!(i & 1) ? 1 : 0));
	static maskPattern2 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!(j % 3) ? 1 : 0));
	static maskPattern3 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!((i + j) % 3) ? 1 : 0));
	static maskPattern4 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!((Math.trunc(i / 2) + Math.trunc(j / 3)) & 1) ? 1 : 0));
	static maskPattern5 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!((i * j) % 6) ? 1 : 0));
	static maskPattern6 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!((((i * j) & 1) + ((i * j) % 3)) & 1) ? 1 : 0));
	static maskPattern7 = GenQRCodeMatrix.makeMaskPattern((i, j) => (!((((i + j) & 1) + ((i * j) % 3)) & 1) ? 1 : 0));
	static maskPattern/*[micro]*/= {
		true: [
			GenQRCodeMatrix.maskPattern1,
			GenQRCodeMatrix.maskPattern4,
			GenQRCodeMatrix.maskPattern6,
			GenQRCodeMatrix.maskPattern7,
		],
		false: [
			GenQRCodeMatrix.maskPattern0,
			GenQRCodeMatrix.maskPattern1,
			GenQRCodeMatrix.maskPattern2,
			GenQRCodeMatrix.maskPattern3,
			GenQRCodeMatrix.maskPattern4,
			GenQRCodeMatrix.maskPattern5,
			GenQRCodeMatrix.maskPattern6,
			GenQRCodeMatrix.maskPattern7,
		],
	}
}

class GenQRCodeGIF {
	constructor(qrcode, scale=2) {
		const qrsize = qrcode.length;
		const padding = ((qrsize < 21) ? 2 : 4);

		const spadding = padding * scale;
		const swidth = (qrsize + padding * 2) * scale;
		const sheight = swidth;

		const x0data = Array(spadding).fill(0);
		const y0data = Array(swidth).fill(0);

		let data = [];
		for (let y = 0; y < spadding; y++)
			data.push.apply(data, y0data);
		for (let x = qrsize - 1; x >= 0; x--) {
			let line = x0data.concat([]);
			for (let y = qrsize - 1; y >= 0; y--) {
				const d = qrcode[y][x];
				for (let ys = 0; ys < scale; ys++)
					line.push(d);
			}
			line.push.apply(line, x0data);
			for (let ys = 0; ys < scale; ys++)
				data.push.apply(data, line);
		}
		for (let y = 0; y < spadding; y++)
			data.push.apply(data, y0data);

		this.S = GenQRCodeGIF;
		this.width = swidth;
		this.height = sheight;
		this.source = data;

		this.binary = [
			0x47, 0x49, 0x46, // Signature
			0x38, 0x39, 0x61, // Version89a

			// Logical Screen Descriptor
			(swidth & 0xff), ((swidth >> 8) & 0xff),
			(sheight & 0xff),((sheight >> 8) & 0xff),
			0x91, 3, 49,

			// Global Color Table
			0xff, 0xff, 0xff,
			0x00, 0x00, 0x00,
			0xdf, 0xdf, 0xdf,
			0xff, 0xff, 0xff,

			// Image Descriptor
			0x2c,
			0x00, 0x00,
			0x00, 0x00,
			(swidth & 0xff), ((swidth >> 8) & 0xff),
			(sheight & 0xff),((sheight >> 8) & 0xff),
			0,
		];

		this.tableBasedImage();

		// Trailer
		this.binary.push(0x3b);
	}

	tableBasedImage() {
		this.buffer = [];
		this.bs_pos = 0;
		this.table = [];
		this.code_max = (1 << 12) - 1;

		const tree = [];
		const tree_size = this.code_max + 1;
		for (let i = 0; i < tree_size; i++)
			tree.push({
				code: i,
				next: -1,
				down: -1,
				data: 0,
			});
		this.tree = tree;

		this.bits_init = 3;
		this.encodeImage();

		this.binary.push(this.bits_init);
		this.pushSubBlock(this.buffer)
	}

	pushSubBlock(data) {
		const buffer = this.buffer;
		let binary = this.binary;
		let offset = 0;
		let size = buffer.length;
		while (size > 0) {
			let count = size;
			if (count > 255) count = 255;
			binary.push(count);
			binary.push.apply(binary, buffer.slice(offset, offset + count));
			offset += count;
			size -= count;
		}
		binary.push(0);
		this.binary = binary;
	}

	encodeImage() {
		const tree = this.tree;
		const bits_init = this.bits_init;
		let bits_curr = bits_init + 1;
		const code_max = this.code_max - 1;
		const code_clear = (1 << bits_init);
		const code_end = code_clear + 1;
		let code_curr = code_end;
		let code_step = (1 << bits_curr);

		const source = this.source;
		this.bs_pos = 0;
		this.buffer = [];
		this.write(code_clear, bits_curr);
		if (!source.length) {
			this.write(code_end, bits_curr);
			return;
		}

		const s_curr = source.values();
		let s_data = s_curr.next();
		for (;;) {
			let c_curr = s_data.value;
			let data = c_curr;
			lzw4: for (;;) {
				s_data = s_curr.next();
				if (s_data.done) {
					this.write(c_curr, bits_curr);
					this.write(code_end, bits_curr);
					return;
				}
				data = s_data.value;
				let next = tree[c_curr].down;
				for (;;) {
					if (next < 0)
						break lzw4;
					const ntree = tree[next];
					if (data == ntree.data)
						break;
					next = ntree.next;
				}
				c_curr = next;
			}
			this.write(c_curr, bits_curr);
			{
				const node = tree[c_curr];
				const next = tree[++code_curr];
				next.code = code_curr;
				next.next = node.down;
				next.down = -1;
				next.data = data;
				node.down = code_curr;
			}
			if (code_curr < code_step)
				continue;
			if (code_curr < code_max) {
				bits_curr++
				code_step <<= 1;
				if (code_step < code_max)
					continue;
				code_step = code_max;
				continue;
			}
			this.write(code_clear, bits_curr);
			this.clearTree();
			bits_curr = bits_init + 1;
			code_curr = code_end;
			code_step = (1 << bits_curr);
		}
	}

	clearTree() {
		const tree = this.tree;
		const tsz = (1 << this.bits_init);
		for (let i = 0; i < tsz; i++) {
			const node = tree[i];
			node.code = i;
			node.next = -1;
			node.down = -1;
			node.data = 0;
		}
	}

	write(data, bits) {
		const buffer = this.buffer;
		const bpos = this.bs_pos;
		let idxs = (bpos >> 3);
		const idxe = ((bpos + bits - 1) >> 3);
		data <<= (bpos & 7);
		if (idxs < buffer.length) {
			buffer[idxs] |= (data & 0xff);
			data >>= 8;
			idxs++;
		}
		while (idxs <= idxe) {
			buffer.push(data & 0xff);
			data >>= 8;
			idxs++;
		}
		this.bs_pos = bpos + bits;
	}
}

class GenQRCodeBase64 {
	static EncodeTable = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
	static DecodeTable = [...Array(256)].map((_, i) =>
		GenQRCodeBase64.EncodeTable.indexOf(String.fromCharCode(i)));

	static encode(b) {
		const table = GenQRCodeBase64.EncodeTable;
		const blen = b.length;
		const brem = blen % 3;
		const bcnt = blen - brem;
		let s = '';
		let i = 0;
		while (i < bcnt) {
			const d0 = b[i++];
			const d1 = b[i++];
			const d2 = b[i++];
			const d = (d0 << 16) | (d1 << 8) | d2;
			s += table[(d >> 18) & 0x3f];
			s += table[(d >> 12) & 0x3f];
			s += table[(d >>  6) & 0x3f];
			s += table[d & 0x3f];
		}
		if (brem) {
			const b2 = (brem == 2);
			const d0 = b[i++];
			const d1 = (b2 ? b[i++] : 0);
			const d = (d0 << 16) | (d1 << 8);
			s += table[(d >> 18) & 0x3f];
			s += table[(d >> 12) & 0x3f];
			s += (b2 ? table[(d >> 6) & 0x3f] : '=');
			s += '=';
		}
		return s;
	}

	static decode(s) {
		const slen = s.length;
		const table = GenQRCodeBase64.DecodeTable;
		const fdata = new Array();
		const b = [];
		for (let i = 0; i < slen; i++) {
			const sc = s.charCodeAt(i);
			if (sc >= 256) continue;
			const cc = table[sc];
			if (cc >= 0) fdata.push(cc)
		}
		const flen = fdata.length;
		for (let i = 0; i < flen; i += 4) {
			const d0 = fdata[i + 0] & 0x3f;
			const d1 = fdata[i + 1] & 0x3f;
			const d2 = fdata[i + 2] & 0x3f;
			const d3 = fdata[i + 3] & 0x3f;
			const d = (d0 << 18) | (d1 << 12) | (d2 << 6) | d3;
			b.push((d >> 16), ((d >> 8) & 255), (d & 255));
		}
		return b.slice(0, b.length - (fdata[flen - 2] >> 6) + (fdata[flen - 1] >> 6));
	}

	static decode16be(s) {
		const b = GenQRCodeBase64.decode(s);
		return [...Array(b.length >> 1)].map((_, i) => (b[i << 1] << 8) | (b[(i << 1) + 1]));
	}

	static decode32be(s) {
		const b = GenQRCodeBase64.decode(s);
		return [...Array(s.length >> 2)].map((_, i) =>
			(b[i << 2] << 24) | (b[(i << 2) + 1] << 16) | (b[(i << 2) + 2] << 8) | (b[(i << 2) + 3]));
	}
}

class GenQRCodeCP932 {
	static MODE_KANJI = 0;
	static MODE_KANA  = 1;
	static MODE_CP932 = 2;

	static ucsToCP932Table = (() => {
		const decoder = new TextDecoder('sjis');
		const buffer = new ArrayBuffer(2);
		const view1 = new DataView(buffer, 0, 1);
		const view2 = new DataView(buffer);
		const ucs = new Map();

		for (let c = 0xA1; c <= 0xDF; ++c) {
			view1.setUint8(0, c);
			const ds = decoder.decode(view1);
			if (ds != undefined) {
				const dc = ds.codePointAt(0);
				if (dc != 0xfffd && !ucs.has(dc))
					ucs.set(dc, c);
			}
		}

		for (let hp = 0xa1; hp <= 0xdc; ++hp) {
			const h = hp ^ 0x20;
			const hh = h << 8;
			view2.setUint8(0, h);
			for (let l = 0x40; l <= 0xfc; ++l) {
				view2.setUint8(1, l);
				const ds = decoder.decode(view2);
				if (ds != undefined) {
					const dc = ds.codePointAt(0);
					if (dc != 0xfffd && !ucs.has(dc))
						ucs.set(dc, hh | l);
				}
			}
		}

		ucs.set(0x301C, 0x8160);
		ucs.set(0x2212, 0x817C);
		ucs.set(0x00A2, 0x8191);
		ucs.set(0x00A3, 0x8192);
		ucs.set(0x00AC, 0x81CA);

		return ucs;
	})();

	static getCode(c) {
		return GenQRCodeCP932.ucsToCP932Table.get(c) ?? -1;
	}

	static getCodeEx(c) {
		if ((c < 0x80) || (0x10000 <= c))
			return undefined;

		const S = GenQRCodeCP932;
		const s = S.getCode(c);

		if (s < 0)
			return undefined;
		if (s < 0x100)
			return { mode: S.MODE_KANA, cp932: s, kanji: undefined };
		if (s >= 0xebc0)
			return { mode: S.MODE_CP932, cp932: s, kanji: undefined };

		// const t = s - ((s < 0xe000) ? 0x8140 : 0xc140);
		const t = (s ^ 0xa000) - 0x2140;
		const k = ((t >> 8) * 192) + (t & 0xff);
		return { mode: S.MODE_KANJI, cp932: s, kanji: k };
	}
}

/*
 *
 */

function createImgSrcWithBase64(b64s) {
	return 'data:image/gif;charset=utf-8;base64,' + b64s;
}

function createImgWithBase64(b64s, id) {
	const priority = 'important';
	const img = document.createElement('img');
	if (id != undefined)
		img.setAttribute('id', id);
	img.setAttribute('class', 'b64img');
	img.setAttribute('src', createImgSrcWithBase64(b64s));
	img.style.setProperty('margin', '0', priority);
	img.style.setProperty('image-rendering', 'pixelated', priority);
	return img;
}

/*
 *
 */

function correctQRScale(scale) {
	scale = Number(scale);
	if (isNaN(scale)) return 2;
	return Math.max(1, Math.min(16, scale));
}

function correctQRCharSet(charset) {
	if (charset == 'ISO' ||
		charset == 'JIS')
		return charset;
	return 'ISO';
}

function correctQRCorrect(correct) {
	if (correct == 'L' ||
		correct == 'M' ||
		correct == 'Q' ||
		correct == 'H')
		return correct;
	return 'Q';
}

function correctQRFixCorrect(fixcorrect) {
	return Boolean(fixcorrect);
}

function getQRScale() {
	return correctQRScale(tagScale.value);
}

function getQRCharSet() {
	return correctQRCharSet((() => {
		if (tagCharSetISO.checked) return tagCharSetISO.value;
		if (tagCharSetJIS.checked) return tagCharSetJIS.value;
	})());
}

function getQRCorrect() {
	return correctQRCorrect((() => {
		if (tagCorrectL.checked) return tagCorrectL.value;
		if (tagCorrectM.checked) return tagCorrectM.value;
		if (tagCorrectQ.checked) return tagCorrectQ.value;
		if (tagCorrectH.checked) return tagCorrectH.value;
	})());
}

function getQRFixCorrect() {
	return correctQRFixCorrect(tagFixCorrect.checked);
}

/*
 *
 */

let extSettings;

function getSettings() {
	extSettings = {
		open: tagSettings.open,
		scale: getQRScale(),
		charset: getQRCharSet(),
		correct: getQRCorrect(),
		fixcorrect: getQRFixCorrect(),
	}
}

function putSettings() {
	tagSettings.open = extSettings.open;
	tagScale.value = correctQRScale(extSettings.scale);

	const charset = correctQRCharSet(extSettings.charset);
	tagCharSetISO.checked = (charset == 'ISO');
	tagCharSetJIS.checked = (charset == 'JIS');

	const correct = correctQRCorrect(extSettings.correct);
	tagCorrectL.checked = (correct == 'L');
	tagCorrectM.checked = (correct == 'M');
	tagCorrectQ.checked = (correct == 'Q');
	tagCorrectH.checked = (correct == 'H');

	const fixcorrect = correctQRFixCorrect(extSettings.fixcorrect);
	tagFixCorrect.checked = fixcorrect;
}

function resetSettings() {
	extSettings.open = false;
	extSettings.scale = 2;
	extSettings.charset = 'ISO';
	extSettings.correct = 'Q';
	extSettings.fixcorrect = true;
	saveSettings();

	extSettings.open = true;
	putSettings();
}

function loadSettings() {
	return Promise.all([
		chrome.storage.sync.get('open'),
		chrome.storage.sync.get('scale'),
		chrome.storage.sync.get('charset'),
		chrome.storage.sync.get('correct'),
		chrome.storage.sync.get('fixcorrect')
	]).then((values) => {
		extSettings.open = values[0].open ?? tagSettings.open;
		extSettings.scale = correctQRScale(values[1].scale) ?? getQRScale();
		extSettings.charset = correctQRCharSet(values[2].charset) ?? getQRCharSet();
		extSettings.correct = correctQRCorrect(values[3].correct) ?? getQRCorrect();
		extSettings.fixcorrect = correctQRFixCorrect(values[4].fixcorrect) ?? getQRFixCorrect();
	});
}

function saveSettings() {
	chrome.storage.sync.set(extSettings);
}

/*
 *
 */

const idQRImage = 'tagQRCodeImage';
let currentQRImage;

function onUpdate() {
	const qrtext = tagInput.value;
	const qrscale = getQRScale();
	const qrcharset = getQRCharSet();
	const qrcorrect = getQRCorrect();
	const qrfixcorrect = getQRFixCorrect();
	let correct = { 'L': 1, 'M': 2, 'Q': 3, 'H': 4 }[qrcorrect];

	while (tagQRCode.firstChild)
		tagQRCode.removeChild(tagQRCode.firstChild);

	if (qrtext.length == 0) {
		currentQRImage = undefined;
		return;
	}

	const qrmessage = new GenQRCode(qrtext);
	const option = {
		size: GenQRCodeType.SizeListNormal,
		jis: qrcharset == 'JIS',
	}
	const scale = Math.min(16, Math.max(1, qrscale));
	const qrimages = qrmessage.autoEncode(option, scale);
	if (qrfixcorrect) {
		while (correct < 4) {
			const qrcurr = qrimages[correct + 0];
			const qrnext = qrimages[correct + 1];
			if (qrcurr.type.size != qrnext.type.size)
				break;
			++correct;
		}
	}
	const qrimage = qrimages[correct];
	const correct_name = ['μ:0%', 'L:7%', 'M:15%', 'Q:25%', 'H:30%'];

	if (qrimage) {
		currentQRImage = qrimage.gif;

		const qrsize = qrimage.type.size;
		const qrtype = Math.trunc(1 + (qrsize - 21) / 4);
		const img = createImgWithBase64(qrimage.base64gif, idQRImage);
		tagQRCode.insertAdjacentHTML(
			'beforeend',
			[
				'<div class="xsmall">',
				'「ドラッグ＆ドロップ」か「右クリック」で取得',
				'</div>',
			].join(''),
		);
		tagQRCode.append(img);
		tagQRCode.insertAdjacentHTML(
			'beforeend',
			[
				'<div class="small center">',
				`${qrtype}型 (${qrsize} x ${qrsize}) ${correct_name[correct]}`,
				'</div>',
				'<div class="xxsmall center">',
				`[GIF:${currentQRImage.binary.length}バイト]`,
				'</div>',
				'<div class="xsmall center">',
				'周辺余白(クワイエットゾーン)は<br>規格で定められた幅があります。',
				'</div>',
			].join(''),
		);
	} else
		tagQRCode.insertAdjacentHTML(
			'beforeend',
			'<div class="error"><strong>QRコードを生成できませんでした。</strong></div>'
		);
}

function onChangeSettings() {
	getSettings();
	saveSettings();
}

function onChange() {
	onChangeSettings();
	onUpdate();
}

function onReset() {
	resetSettings();
	onUpdate();
}

/*
 *
 */

function onLoad() {

	tagInput.oninput = onUpdate;
	tagSettings.ontoggle = onChangeSettings;
	tagScale.oninput = onChange;
	[
		tagCharSetISO, tagCharSetJIS,
		tagCorrectL, tagCorrectM, tagCorrectQ, tagCorrectH,
		tagFixCorrect,
	].forEach((tag) => tag.onclick = onChange);
	tagReset.onclick = onReset;
	// tagInfoSub.onclick = (() => { tagInfo.open = !tagInfo.open; });

	getSettings();
	Promise.all([
		chrome.tabs.query({
			active: true,
			currentWindow: true,
		}),
		loadSettings(),
	]).then((values) => {
		const tabs = values[0];
		if (tabs && tabs.length == 1) {
			const tab = tabs[0];
			tagInput.innerText = tab.url ?? '';
		}
		putSettings();
		onUpdate();

		tagInput.focus();
		tagInput.select();
	});
}

window.onload = onLoad;
