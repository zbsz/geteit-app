package com.geteit.image;

import android.content.ContentResolver;
import android.database.Cursor;
import android.graphics.*;
import android.graphics.Bitmap.Config;
import android.graphics.BitmapFactory.Options;
import android.net.Uri;
import android.provider.MediaStore;
import android.provider.MediaStore.Images.ImageColumns;
import android.util.FloatMath;
import android.util.Log;

import java.io.*;

public class BitmapUtils {

    private static final String TAG = "BitmapUtils";
    private static final int UNCONSTRAINED = -1;
    
    private static Paint scalePaint = new Paint();
    
    static {
        scalePaint.setFilterBitmap(true);
        scalePaint.setAntiAlias(true);
    }
    
    public static int MAX_SIZE = 1024 * 1024; // max number of pixels
    
    public static final Bitmap EMPTY_BITMAP = Bitmap.createBitmap(1, 1, Config.ARGB_8888);
    
    
    public static Bitmap load(ContentResolver contentResolver, Uri uri) {
        return load(contentResolver, uri, UNCONSTRAINED, MAX_SIZE);
    }
    
    public static Bitmap load(ContentResolver contentResolver, Uri uri, int minSideLen, int maxSize) {
        
        Bitmap result = EMPTY_BITMAP;
        try {
            Options opts = new Options();
            opts.inJustDecodeBounds = true;

            BitmapFactory.decodeStream(contentResolver.openInputStream(uri), null, opts);

            result = load(contentResolver.openInputStream(uri), computeSampleSize(opts, minSideLen, maxSize));

            if (isMediaUri(uri) && result != null && result != EMPTY_BITMAP) {
                result  = rotateMediaImage(contentResolver, uri, result);
            }

        } catch (OutOfMemoryError e) {
            Log.e("LoadImage", e.getMessage(), e);
        } catch (IOException e) {
            Log.e("LoadImage", e.getMessage(), e);
        }
        if (result == null) {
            result = EMPTY_BITMAP;
        }
        return result;
    }

    private static boolean isMediaUri(Uri uri) {
        return uri != null && ContentResolver.SCHEME_CONTENT.equals(uri.getScheme()) && MediaStore.AUTHORITY.equals(uri.getAuthority());
    }

    public static void decodeSize(ContentResolver contentResolver, Uri uri, Point out) throws IOException {
        Options opts = new Options();
        opts.inJustDecodeBounds = true;

        BitmapFactory.decodeStream(contentResolver.openInputStream(uri), null, opts);

        out.set(opts.outWidth, opts.outHeight);
    }

    private static Bitmap rotateMediaImage(ContentResolver contentResolver, Uri uri, Bitmap image) throws FileNotFoundException {

        Cursor c = contentResolver.query(uri, null, null, null, null);
        try {
            if (c != null) {
                if (c.moveToFirst()) {

                    int index = c.getColumnIndex(ImageColumns.ORIENTATION);
                    int degrees = index < 0 ? 0 : c.getInt(index);

                    if (degrees != 0) {
                        return rotate(image, degrees);
                    }
                    return image;
                }
            }
        } finally {
            if (c != null) {
                c.close();
            }
        }
        return image;
    }

    public static Bitmap load(File file, int minSideLen, int maxSize) throws IOException {

        FileInputStream is = null;
        try {
            Options opts = new Options();
            opts.inJustDecodeBounds = true;

            is = new FileInputStream(file);
            BitmapFactory.decodeStream(is, null, opts);
            is.close();

            is = new FileInputStream(file);
            return load(is, computeSampleSize(opts, minSideLen, maxSize));
        } finally {
            if (is != null) {
                is.close();
            }
        }
    }

    public static Bitmap load(byte[] data, int inSampleSize) {
        try {
            Options opts = new Options();
            opts.inSampleSize = inSampleSize;
            opts.inDither = false;
            opts.inPreferredConfig = Config.ARGB_8888;

            Bitmap result = BitmapFactory.decodeByteArray(data, 0, data.length, opts);
            return result == null ? EMPTY_BITMAP : result;
        } catch (OutOfMemoryError e) {
            Log.e("LoadImage", e.getMessage(), e);
        }
        return EMPTY_BITMAP;
    }

    public static Bitmap load(InputStream inputStream, int inSampleSize) {
        try {
            Options opts = new Options();
            opts.inSampleSize = inSampleSize;
            opts.inDither = false;
            opts.inPreferredConfig = Config.ARGB_8888;

            Bitmap result = BitmapFactory.decodeStream(inputStream, null, opts);
            return result == null ? EMPTY_BITMAP : result;
        } catch (OutOfMemoryError e) {
            Log.e("LoadImage", e.getMessage(), e);
        }
        return EMPTY_BITMAP;
    }

    public static Bitmap rotate(Bitmap b, int degrees) {
        if (degrees != 0 && b != null) {
            Matrix m = new Matrix();
            m.setRotate(degrees,
                    (float) b.getWidth() / 2, (float) b.getHeight() / 2);
            try {
                Bitmap b2 = Bitmap.createBitmap(
                        b, 0, 0, b.getWidth(), b.getHeight(), m, true);
                if (b != b2) {
                    b.recycle();
                    b = b2;
                }
            } catch (OutOfMemoryError ex) {
                // We have no memory to rotate. Return the original bitmap.
            }
        }
        return b;
    }

    public static Bitmap scale(Bitmap b, float scale) {
        Bitmap res = Bitmap.createBitmap((int)(b.getWidth() * scale + .5f), (int)(b.getHeight() * scale + .5f), b.getConfig());
        Canvas c = new Canvas(res);
        c.scale(scale, scale);
        c.drawBitmap(b, 0, 0, scalePaint);
        return res;
    }

    /*
     * Compute the sample size as a function of minSideLength
     * and maxNumOfPixels.
     * minSideLength is used to specify that minimal width or height of a
     * bitmap.
     * maxNumOfPixels is used to specify the maximal size in pixels that is
     * tolerable in terms of memory usage.
     *
     * The function returns a sample size based on the constraints.
     * Both size and minSideLength can be passed in as IImage.UNCONSTRAINED,
     * which indicates no care of the corresponding constraint.
     * The functions prefers returning a sample size that
     * generates a smaller bitmap, unless minSideLength = IImage.UNCONSTRAINED.
     *
     * Also, the function rounds up the sample size to a power of 2 or multiple
     * of 8 because BitmapFactory only honors sample size this way.
     * For example, BitmapFactory downsamples an image by 2 even though the
     * request is 3. So we round up the sample size to avoid OOM.
     */
    public static int computeSampleSize(Options options, int minSideLength, int maxNumOfPixels) {
        int initialSize = computeInitialSampleSize(options, minSideLength,
                maxNumOfPixels);

        int roundedSize = nextPowerOfTwo(initialSize);
        return roundedSize > initialSize ? roundedSize / 2 : roundedSize;
    }

    public static int nextPowerOfTwo(int value) {
        if(value == 0) {
            return 1;
        } else {
            --value;
            value |= value >> 1;
            value |= value >> 2;
            value |= value >> 4;
            value |= value >> 8;
            value |= value >> 16;
            return value + 1;
        }
    }

    private static int computeInitialSampleSize(Options options, int minSideLength, int maxNumOfPixels) {
        double w = options.outWidth;
        double h = options.outHeight;

        int lowerBound = (maxNumOfPixels == UNCONSTRAINED) ? 1 :
                (int) Math.ceil(Math.sqrt(w * h / maxNumOfPixels));
        int upperBound = (minSideLength == UNCONSTRAINED) ? 128 :
                (int) Math.min(Math.floor(w / minSideLength),
                Math.floor(h / minSideLength));

        if (upperBound < lowerBound) {
            // return the larger one when there is no overlapping zone.
            return lowerBound;
        }

        if ((maxNumOfPixels == UNCONSTRAINED) &&
                (minSideLength == UNCONSTRAINED)) {
            return 1;
        } else if (minSideLength == UNCONSTRAINED) {
            return lowerBound;
        } else {
            return upperBound;
        }
    }

    /**
     * Simplified version.
     * 
     * @param size
     * @param maxSize
     * @return
     */
    public static int getInSampleSize(long size, long maxSize) {
        int scale = (int) FloatMath.sqrt((float) size / maxSize + 1);
        int inSampleSize = 1;
        while (inSampleSize < scale) {
            inSampleSize *= 2;
        }
        Log.d(TAG, "inSampleSize: " + inSampleSize + "  scale: " + scale);
        return inSampleSize;
    }
    
    public static boolean isEmpty(Bitmap image) {
        return image == null || image == EMPTY_BITMAP || (image.getWidth() == 1 && image.getHeight() == 1);
    }
    
    /**
     * Creates a centered bitmap of the desired size.
     *
     * @param source original bitmap source
     * @param width targeted width
     * @param height targeted height
     */
    public static Bitmap extractThumbnail(Bitmap source, int width, int height, boolean recycle) {
        if (source == null || source == BitmapUtils.EMPTY_BITMAP) {
            return BitmapUtils.EMPTY_BITMAP;
        }

        float scale;
        if (source.getWidth() < source.getHeight()) {
            scale = width / (float) source.getWidth();
        } else {
            scale = height / (float) source.getHeight();
        }
        Matrix matrix = new Matrix();
        matrix.setScale(scale, scale);
        return transform(matrix, source, width, height, recycle);
    }
    
    
    public static Bitmap crop(Bitmap source, Rect bounds) {
        if (source == null || source == BitmapUtils.EMPTY_BITMAP) {
            return BitmapUtils.EMPTY_BITMAP;
        }
        
        int x = Math.max(0, bounds.left);
        int y = Math.max(0, Math.min(bounds.top, bounds.bottom));
        int w = Math.min(source.getWidth() - x, bounds.width());
        int h = Math.min(source.getHeight() - y, Math.abs(bounds.height()));
        
        return Bitmap.createBitmap(source, x, y, w, h);
    }
    
    public static Bitmap crop(Bitmap source, int x, int y, int w, int h) {
        if (source == null || source == BitmapUtils.EMPTY_BITMAP) {
            return BitmapUtils.EMPTY_BITMAP;
        }
        
        if (x < 0) {
            x = 0;
        }
        if (y < 0) {
            y = 0;
        }
        
        return Bitmap.createBitmap(source, x, y, Math.min(source.getWidth() - x, w), Math.min(source.getHeight() - y, h));
    }

    /**
     * Transform source Bitmap to targeted width and height.
     */
    private static Bitmap transform(Matrix scaler, Bitmap source, int targetWidth, int targetHeight, boolean recycle) {

        float bitmapWidthF = source.getWidth();
        float bitmapHeightF = source.getHeight();

        float bitmapAspect = bitmapWidthF / bitmapHeightF;
        float viewAspect   = (float) targetWidth / targetHeight;

        if (bitmapAspect > viewAspect) {
            float scale = targetHeight / bitmapHeightF;
            if (scale < .9F || scale > 1F) {
                scaler.setScale(scale, scale);
            } else {
                scaler = null;
            }
        } else {
            float scale = targetWidth / bitmapWidthF;
            if (scale < .9F || scale > 1F) {
                scaler.setScale(scale, scale);
            } else {
                scaler = null;
            }
        }

        Bitmap b1;
        if (scaler != null) {
            // this is used for minithumb and crop, so we want to filter here.
            b1 = Bitmap.createBitmap(source, 0, 0,
            source.getWidth(), source.getHeight(), scaler, true);
        } else {
            b1 = source;
        }

        if (recycle && b1 != source) {
            source.recycle();
        }

        int dx1 = Math.max(0, b1.getWidth() - targetWidth);
        int dy1 = Math.max(0, b1.getHeight() - targetHeight);

        Bitmap b2 = Bitmap.createBitmap(b1, dx1 / 2, dy1 / 2, targetWidth, targetHeight, null, true);

        if (b2 != b1) {
            if (recycle || b1 != source) {
                b1.recycle();
            }
        }

        return b2;
    }
    
    public static void blur(Bitmap bitmap, int radius) {

        // Stack Blur v1.0 from
        // http://www.quasimondo.com/StackBlurForCanvas/StackBlurDemo.html
        //
        // Java Author: Mario Klingemann <mario at quasimondo.com>
        // http://incubator.quasimondo.com
        // created Feburary 29, 2004
        // Android port : Yahel Bouaziz <yahel at kayenko.com>
        // http://www.kayenko.com
        // ported april 5th, 2012

        // This is a compromise between Gaussian Blur and Box blur
        // It creates much better looking blurs than Box Blur, but is
        // 7x faster than my Gaussian Blur implementation.
        //
        // I called it Stack Blur because this describes best how this
        // filter works internally: it creates a kind of moving stack
        // of colors whilst scanning through the image. Thereby it
        // just has to add one new block of color to the right side
        // of the stack and remove the leftmost color. The remaining
        // colors on the topmost layer of the stack are either added on
        // or reduced by one, depending on if they are on the right or
        // on the left side of the stack.
        //
        // If you are using this algorithm in your code please add
        // the following line:
        //
        // Stack Blur Algorithm by Mario Klingemann <mario@quasimondo.com>

//        Bitmap bitmap = sentBitmap.copy(sentBitmap.getConfig(), true);

        if (radius < 1) {
            return;
        }

        int w = bitmap.getWidth();
        int h = bitmap.getHeight();

        int[] pix = new int[w * h];
        Log.d(TAG, w + " " + h + " " + pix.length);
        bitmap.getPixels(pix, 0, w, 0, 0, w, h);

        int wm = w - 1;
        int hm = h - 1;
        int wh = w * h;
        int div = radius + radius + 1;

        int r[] = new int[wh];
        int g[] = new int[wh];
        int b[] = new int[wh];
        int rsum, gsum, bsum, x, y, i, p, yp, yi, yw;
        int vmin[] = new int[Math.max(w, h)];

        int divsum = (div + 1) >> 1;
        divsum *= divsum;
        int dv[] = new int[256 * divsum];
        for (i = 0; i < 256 * divsum; i++) {
            dv[i] = (i / divsum);
        }

        yw = yi = 0;

        int[][] stack = new int[div][3];
        int stackpointer;
        int stackstart;
        int[] sir;
        int rbs;
        int r1 = radius + 1;
        int routsum, goutsum, boutsum;
        int rinsum, ginsum, binsum;

        for (y = 0; y < h; y++) {
            rinsum = ginsum = binsum = routsum = goutsum = boutsum = rsum = gsum = bsum = 0;
            for (i = -radius; i <= radius; i++) {
                p = pix[yi + Math.min(wm, Math.max(i, 0))];
                sir = stack[i + radius];
                sir[0] = (p & 0xff0000) >> 16;
                sir[1] = (p & 0x00ff00) >> 8;
                sir[2] = (p & 0x0000ff);
                rbs = r1 - Math.abs(i);
                rsum += sir[0] * rbs;
                gsum += sir[1] * rbs;
                bsum += sir[2] * rbs;
                if (i > 0) {
                    rinsum += sir[0];
                    ginsum += sir[1];
                    binsum += sir[2];
                } else {
                    routsum += sir[0];
                    goutsum += sir[1];
                    boutsum += sir[2];
                }
            }
            stackpointer = radius;

            for (x = 0; x < w; x++) {

                r[yi] = dv[rsum];
                g[yi] = dv[gsum];
                b[yi] = dv[bsum];

                rsum -= routsum;
                gsum -= goutsum;
                bsum -= boutsum;

                stackstart = stackpointer - radius + div;
                sir = stack[stackstart % div];

                routsum -= sir[0];
                goutsum -= sir[1];
                boutsum -= sir[2];

                if (y == 0) {
                    vmin[x] = Math.min(x + radius + 1, wm);
                }
                p = pix[yw + vmin[x]];

                sir[0] = (p & 0xff0000) >> 16;
                sir[1] = (p & 0x00ff00) >> 8;
                sir[2] = (p & 0x0000ff);

                rinsum += sir[0];
                ginsum += sir[1];
                binsum += sir[2];

                rsum += rinsum;
                gsum += ginsum;
                bsum += binsum;

                stackpointer = (stackpointer + 1) % div;
                sir = stack[(stackpointer) % div];

                routsum += sir[0];
                goutsum += sir[1];
                boutsum += sir[2];

                rinsum -= sir[0];
                ginsum -= sir[1];
                binsum -= sir[2];

                yi++;
            }
            yw += w;
        }
        for (x = 0; x < w; x++) {
            rinsum = ginsum = binsum = routsum = goutsum = boutsum = rsum = gsum = bsum = 0;
            yp = -radius * w;
            for (i = -radius; i <= radius; i++) {
                yi = Math.max(0, yp) + x;

                sir = stack[i + radius];

                sir[0] = r[yi];
                sir[1] = g[yi];
                sir[2] = b[yi];

                rbs = r1 - Math.abs(i);

                rsum += r[yi] * rbs;
                gsum += g[yi] * rbs;
                bsum += b[yi] * rbs;

                if (i > 0) {
                    rinsum += sir[0];
                    ginsum += sir[1];
                    binsum += sir[2];
                } else {
                    routsum += sir[0];
                    goutsum += sir[1];
                    boutsum += sir[2];
                }

                if (i < hm) {
                    yp += w;
                }
            }
            yi = x;
            stackpointer = radius;
            for (y = 0; y < h; y++) {
                pix[yi] = 0xff000000 | (dv[rsum] << 16) | (dv[gsum] << 8)
                        | dv[bsum];

                rsum -= routsum;
                gsum -= goutsum;
                bsum -= boutsum;

                stackstart = stackpointer - radius + div;
                sir = stack[stackstart % div];

                routsum -= sir[0];
                goutsum -= sir[1];
                boutsum -= sir[2];

                if (x == 0) {
                    vmin[y] = Math.min(y + r1, hm) * w;
                }
                p = x + vmin[y];

                sir[0] = r[p];
                sir[1] = g[p];
                sir[2] = b[p];

                rinsum += sir[0];
                ginsum += sir[1];
                binsum += sir[2];

                rsum += rinsum;
                gsum += ginsum;
                bsum += binsum;

                stackpointer = (stackpointer + 1) % div;
                sir = stack[stackpointer];

                routsum += sir[0];
                goutsum += sir[1];
                boutsum += sir[2];

                rinsum -= sir[0];
                ginsum -= sir[1];
                binsum -= sir[2];

                yi += w;
            }
        }

        Log.d(TAG, w + " " + h + " " + pix.length);
        bitmap.setPixels(pix, 0, w, 0, 0, w, h);
    }
}
