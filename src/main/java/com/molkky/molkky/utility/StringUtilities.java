package com.molkky.molkky.utility;

import java.security.SecureRandom;

public class StringUtilities {
    public static String createCode(int n){
        String charSet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWYXZ";
        SecureRandom random = new SecureRandom(); // Compliant for security-sensitive use cases
        StringBuilder buff = new StringBuilder(n);
        for(int i=0;i<n;i++) {
            int offset = random.nextInt(charSet.length());
            buff.append(charSet.charAt(offset));
        }
        return buff.toString();
    }

    private StringUtilities(){}
}
