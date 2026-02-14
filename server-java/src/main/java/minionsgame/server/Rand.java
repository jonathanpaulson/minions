/**
 * Rand.java
 * A simple class for random number generation, independent of the java built-in random.
 * High-period and high-quality.
 */
package minionsgame.server;

import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

public class Rand {
    public final String seed;
    private final Xoroshiro128plus xoro;
    private final XorShift1024Mult xors;
    private final PCG32 pcg;

    private Rand(long[] seeds) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < seeds.length; i++) {
            if (i > 0) sb.append("|");
            sb.append(Long.toString(seeds[i]));
        }
        this.seed = sb.toString();
        this.xoro = new Xoroshiro128plus(seed);
        this.xors = new XorShift1024Mult(seed);
        this.pcg = new PCG32(seed);
    }

    public static Rand create() {
        return new Rand(RandUtils.makeSeedsFromTime("Rand", 2));
    }

    public static Rand create(long seed) {
        return new Rand(new long[]{seed});
    }

    public static Rand create(long[] seeds) {
        return new Rand(seeds);
    }

    public long nextLong() {
        return (xoro.next() ^ xors.next()) + pcg.next();
    }

    public int nextInt() {
        return (int) nextLong();
    }

    public int nextInt(int n) {
        if (n <= 0) throw new RuntimeException("Rand.nextInt(n): non-positive n: " + n);
        int x, y;
        do {
            x = nextInt() & 0x7FFFFFFF;
            y = x % n;
        } while (x - y + (n - 1) < 0);
        return y;
    }

    public double nextDouble() {
        return (nextLong() & 0x1FFFFFFFFFFFFFL) / (double) (1L << 53);
    }

    public <T> void shuffleInplace(T[] arr) {
        for (int i = 1; i < arr.length; i++) {
            int j = nextInt(i + 1);
            T tmp = arr[i];
            arr[i] = arr[j];
            arr[j] = tmp;
        }
    }

    public <T> List<T> shuffle(List<T> list) {
        @SuppressWarnings("unchecked")
        T[] arr = (T[]) list.toArray();
        shuffleInplace(arr);
        List<T> result = new ArrayList<>(arr.length);
        for (T item : arr) {
            result.add(item);
        }
        return result;
    }
}

interface LongGen {
    long next();
}

/**
 * xoroshiro128plus from http://xoroshiro.di.unimi.it/
 * Initial values must be not both zero
 * Period = 2^128 - 1
 * Not threadsafe
 */
class Xoroshiro128plus implements LongGen {
    private long x0;
    private long x1;

    public Xoroshiro128plus(long initX0, long initX1) {
        this.x0 = initX0;
        this.x1 = initX1;
    }

    public Xoroshiro128plus(String seed) {
        long[] seeds = RandUtils.makeNonzeroSeedsFromSeed("Xoroshiro128plus", seed, 2);
        this.x0 = seeds[0];
        this.x1 = seeds[1];
    }

    private static long rotateLeft(long x, int i) {
        return (x << i) | (x >>> (64 - i));
    }

    public long next() {
        long s0 = x0;
        long s1 = x1;

        s1 ^= s0;
        x0 = rotateLeft(s0, 55) ^ s1 ^ (s1 << 14);
        x1 = rotateLeft(s1, 36);

        return x0 + x1;
    }
}

/**
 * xorshift1024* from http://xoroshiro.di.unimi.it/
 * Not all values should be zero
 * Period = 2^1024 - 1
 * Not threadsafe
 */
class XorShift1024Mult implements LongGen {
    private static final int LEN = 16;
    private final long[] s;
    private int idx;

    public XorShift1024Mult(long[] initS) {
        if (initS.length != LEN)
            throw new RuntimeException("XorShift1024Mult initialized with array initS of length != 16");
        this.s = initS.clone();
        this.idx = 0;
    }

    public XorShift1024Mult(String seed) {
        this(RandUtils.makeNonzeroSeedsFromSeed("XorShift1024Mult", seed, 16));
    }

    public long next() {
        long s0 = s[idx];
        idx = (idx + 1) % LEN;
        long s1 = s[idx];
        s1 ^= s1 << 31;
        s[idx] = s1 ^ s0 ^ (s1 >>> 11) ^ (s0 >>> 30);
        return s[idx] * 1181783497276652981L;
    }
}

/**
 * PCG Generator from http://www.pcg-random.org/
 * Period = 2^64
 * Not threadsafe
 */
class PCG32 implements LongGen {
    private long s;

    public PCG32(long initS) {
        this.s = initS;
    }

    public PCG32(String seed) {
        this(RandUtils.makeNonzeroSeedsFromSeed("PCG32", seed, 1)[0]);
    }

    public int nextInt() {
        s = s * 6364136223846793005L + 1442695040888963407L;
        int x = (int) (((s >>> 18) ^ s) >>> 27);
        int rot = (int) (s >>> 59);
        return (x >>> rot) | (x << (32 - rot));
    }

    public long next() {
        int low = nextInt();
        int high = nextInt();
        return ((long) low & 0xFFFFFFFFL) ^ ((long) high << 32);
    }
}

class RandUtils {
    public static byte[] sha256Bytes(String s) {
        try {
            return MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8"));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static String sha256(String s) {
        byte[] bytes = sha256Bytes(s);
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02X", b & 0xff));
        }
        return sb.toString();
    }

    public static long sha256Long(String s) {
        return bytesToLongs(sha256Bytes("sha256Long" + sha256(s)))[0];
    }

    public static long[] bytesToLongs(byte[] bytes) {
        int numLongs = bytes.length / 8;
        long[] longs = new long[numLongs];
        for (int i = 0; i < numLongs; i++) {
            longs[i] =
                ((long)(bytes[i*8+0] & 0xFF))       |
                ((long)(bytes[i*8+1] & 0xFF) <<  8)  |
                ((long)(bytes[i*8+2] & 0xFF) << 16)  |
                ((long)(bytes[i*8+3] & 0xFF) << 24)  |
                ((long)(bytes[i*8+4] & 0xFF) << 32)  |
                ((long)(bytes[i*8+5] & 0xFF) << 40)  |
                ((long)(bytes[i*8+6] & 0xFF) << 48)  |
                ((long)(bytes[i*8+7] & 0xFF) << 56);
        }
        return longs;
    }

    private static final AtomicLong counter = new AtomicLong();

    public static long[] makeSeedsFromTime(String salt, int num) {
        int len = (num + 3) / 4;
        String hashsalt = sha256(salt);
        byte[] allBytes = new byte[len * 32];
        for (int i = 0; i < len; i++) {
            String hashStr = "fromtime:" + i + ":" + counter.incrementAndGet() + ":" + System.nanoTime() + ":" + hashsalt;
            byte[] hashBytes = sha256Bytes(hashStr);
            System.arraycopy(hashBytes, 0, allBytes, i * 32, 32);
        }
        long[] allLongs = bytesToLongs(allBytes);
        long[] result = new long[num];
        System.arraycopy(allLongs, 0, result, 0, num);
        return result;
    }

    public static long[] makeNonzeroSeedsFromSeed(String salt, String seed, int num) {
        int len = (num + 3) / 4;
        String hashsalt = sha256(salt);

        for (int tries = 0; ; tries++) {
            byte[] allBytes = new byte[len * 32];
            for (int i = 0; i < len; i++) {
                String hashStr = "fromseed:" + tries + ":" + i + ":" + hashsalt + ":" + seed;
                byte[] hashBytes = sha256Bytes(hashStr);
                System.arraycopy(hashBytes, 0, allBytes, i * 32, 32);
            }
            long[] allLongs = bytesToLongs(allBytes);
            long[] result = new long[num];
            System.arraycopy(allLongs, 0, result, 0, num);

            boolean hasNonzero = false;
            for (long x : result) {
                if (x != 0) { hasNonzero = true; break; }
            }
            if (hasNonzero) return result;
        }
    }
}
