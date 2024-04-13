import java.util.ArrayList;
import java.util.List;

abstract class Number {
    abstract Number add(Number n);
    public abstract String toString();
}

class ComplexNumber extends Number {
    private double real;
    private double imag;

    public ComplexNumber(double real, double imag) {
        this.real = real;
        this.imag = imag;
    }

    public double getRealPart() {
        return real;
    }

    public double getImaginaryPart() {
        return imag;
    }

    public Number add(Number n) {
        if (n instanceof ComplexNumber) {
            ComplexNumber cn = (ComplexNumber) n;
            return new ComplexNumber(this.real + cn.getRealPart(), this.imag + cn.getImaginaryPart());
        }
        return this;
    }

    public String toString() {
        return String.format("%.4f + %.4fi", real, imag);
    }
}

class GaussianInteger extends ComplexNumber {
    private int real;
    private int imag;

    public GaussianInteger(int real, int imag) {
        super(real, imag);
        this.real = real;
        this.imag = imag;
    }

    public Number add(Number n) {
        if (n instanceof GaussianInteger) {
            GaussianInteger gi = (GaussianInteger) n;
            return new GaussianInteger(this.real + gi.real, this.imag + gi.imag);
        }
        return super.add(n);
    }

    public String toString() {
        return real + " + " + imag + "i";
    }
}

class RealNumber extends ComplexNumber {
    private double real;

    public RealNumber(double real) {
        super(real, 0.0);
        this.real = real;
    }

    public Number add(Number n) {
        if (n instanceof RealNumber) {
            RealNumber rn = (RealNumber) n;
            return new RealNumber(this.real + rn.real);
        }
        return super.add(n);
    }

    public String toString() {
        return String.format("%.1f", real);
    }
}

class RationalNumber extends RealNumber {
    private int numerator;
    private int denominator;

    public RationalNumber(int numerator, int denominator) {
        super((double) numerator / denominator);
        this.numerator = numerator;
        this.denominator = denominator;
        this.reduce();
    }

    public int getNumerator() {
        return numerator;
    }

    public int getDenominator() {
        return denominator;
    }

    public Number add(Number n) {
        if (n instanceof RationalNumber) {
            RationalNumber rn = (RationalNumber) n;
            int newNumerator = this.numerator * rn.getDenominator() + rn.getNumerator() * this.denominator;
            int newDenominator = this.denominator * rn.getDenominator();
            return new RationalNumber(newNumerator, newDenominator);
        }
        return super.add(n);
    }

    public String toString() {
        return numerator + " / " + denominator;
    }

    private void reduce() {
        int gcd = gcd(numerator, denominator);
        numerator /= gcd;
        denominator /= gcd;
    }

    private int gcd(int a, int b) {
        return b == 0 ? a : gcd(b, a % b);
    }
}

class IntegerNumber extends RationalNumber {
    private int value;

    public IntegerNumber(int value) {
        super(value, 1);
        this.value = value;
    }

    public Number add(Number n) {
        if (n instanceof IntegerNumber) {
            IntegerNumber in = (IntegerNumber) n;
            return new IntegerNumber(this.value + in.value);
        }
        return super.add(n);
    }

    public String toString() {
        return String.valueOf(value);
    }
}

class NaturalNumber extends IntegerNumber {
    private int value;

    public NaturalNumber(int value) {
        super(value);
        if (value < 0) {
            throw new IllegalArgumentException("Natural number must be non-negative.");
        }
        this.value = value;
    }

    public Number add(Number n) {
        if (n instanceof NaturalNumber) {
            NaturalNumber nn = (NaturalNumber) n;
            return new NaturalNumber(this.value + nn.value);
        }
        return super.add(n);
    }
}

public class NumberHierarchy {
    public static void main(String[] args) {

        ComplexNumber c1 = new ComplexNumber(-3.1532, 67.123);
        ComplexNumber c2 = new ComplexNumber(9.0003, 0.193);
        GaussianInteger g1 = new GaussianInteger(-3, 67);
        GaussianInteger g2 = new GaussianInteger(9, 3);
        RealNumber r1 = new RealNumber(5.0);
        RealNumber r2 = new RealNumber(6.5);
        RationalNumber rat1 = new RationalNumber(1, 2);
        RationalNumber rat2 = new RationalNumber(3, 4);
        IntegerNumber i1 = new IntegerNumber(5);
        IntegerNumber i2 = new IntegerNumber(6);
        NaturalNumber n1 = new NaturalNumber(7);
        NaturalNumber n2 = new NaturalNumber(8);

        String[] labels = {
            "Add complex:",
            "Add Gaussian:",
            "Add real:",
            "Add rational:",
            "Add integer:",
            "Add natural:"
        };

        String[] operations = {
            c1 + " + " + c2,
            g1 + " + " + g2,
            r1 + " + " + r2,
            rat1 + " + " + rat2,
            i1 + " + " + i2,
            n1 + " + " + n2
        };

        Number[] results = {
            c1.add(c2),
            g1.add(g2),
            r1.add(r2),
            rat1.add(rat2),
            i1.add(i2),
            n1.add(n2)
        };

        // Find the longest string to determine padding
        int maxOperationLength = 0;
        for (String operation : operations) {
            maxOperationLength = Math.max(maxOperationLength, operation.length());
        }

        // Calculate the maximum length among the label strings to ensure consistent padding and alignment in the output
        int maxLabelLength = 0;
        for (String label : labels) {
            maxLabelLength = Math.max(maxLabelLength, label.length());
        }

        System.out.println("");

        // Print the operations and results, right-aligned
        for (int i = 0; i < operations.length; i++) {
            String formattedOperation = String.format("%" + maxOperationLength + "s", operations[i]);
            String format = "%-" + maxLabelLength + "s %s = %s%n";
            System.out.printf(format, labels[i], formattedOperation, results[i]);
        }

        System.out.println("");
    }
}
