package plt.types;

import net.dclausen.microfloat.MicroDouble;

public class FloatPoint implements Number {
    long f;

    private FloatPoint(long n) {
	this.f = n;
    }

    public static FloatPoint fromInt(int n) {
	return new FloatPoint(MicroDouble.intToDouble(n));
    }

    public static FloatPoint fromString(String s) {
	return new FloatPoint(MicroDouble.parseDouble(s));
    }

    public static FloatPoint PI = new FloatPoint(MicroDouble.PI);
    public static FloatPoint E = new FloatPoint(MicroDouble.E);


    public int toInt() {
	return MicroDouble.intValue(this.f);
    }

    public boolean isInteger() {
	return MicroDouble.truncate(this.f) == this.f;
    }

    public boolean isZero() {
	return MicroDouble.isZero(this.f);
    }

    public boolean numericGreaterThan(Number other) {
	return MicroDouble.gt(this.f, ((FloatPoint)other).f);
    }

    public boolean numericEqual(Number other) {
	return MicroDouble.eq(this.f, ((FloatPoint)other).f);
    }

    public boolean numericLessThan(Number other) {
	return MicroDouble.lt(this.f, ((FloatPoint)other).f);
    }

    
    public Number numericMultiply(Number _other) {
	FloatPoint other = (FloatPoint) _other;
	return new FloatPoint(MicroDouble.mul(this.f, other.f));
    }

    public Number numericDivide(Number _other) {
	FloatPoint other = (FloatPoint) _other;
	return new FloatPoint(MicroDouble.div(this.f, other.f));
    }

    public Number numericPlus(Number _other) {
	FloatPoint other = (FloatPoint) _other;
	return new FloatPoint(MicroDouble.add(this.f, other.f));
    }

    public Number numericMinus(Number _other) {
	FloatPoint other = (FloatPoint) _other;
	return new FloatPoint(MicroDouble.sub(this.f, other.f));
    }

    public Number abs() {
	return new FloatPoint(MicroDouble.abs(this.f));
    }

    public Number sqrt() {
	return new FloatPoint(MicroDouble.sqrt(this.f));
    }

    public Number sin() {
	return new FloatPoint(MicroDouble.sin(this.f));
    }

    public Number cos() {
	return new FloatPoint(MicroDouble.cos(this.f));
    }

    public Number tan() {
	return new FloatPoint(MicroDouble.tan(this.f));
    }

    public Number atan() {
	return new FloatPoint(MicroDouble.atan(this.f));
    }

    public Number asin() {
	return new FloatPoint(MicroDouble.asin(this.f));
    }

    public Number acos() {
	return new FloatPoint(MicroDouble.acos(this.f));
    }

    public Number log() {
	return new FloatPoint(MicroDouble.log(this.f));
    }

    public Number modulo(Number _other) {
	FloatPoint other = (FloatPoint) _other;
	return new FloatPoint(MicroDouble.mod(this.f, other.f));
    }


    public Number floor() {
	return new FloatPoint(MicroDouble.floor(this.f));
    }

    public Number ceiling() {
	return new FloatPoint(MicroDouble.ceil(this.f));
    }


    public String toString() {
	return MicroDouble.toString(this.f);
    }


    public boolean equals(Object other) {
	if (other instanceof FloatPoint) {
	    return this.f == ((FloatPoint)other).f;
	} else {
	    return false;
	}
    }

}
