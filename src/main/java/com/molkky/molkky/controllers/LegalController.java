package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class LegalController extends DefaultAttributes {
    @GetMapping("/politiqueConfidentialite")
    public String politique() {
        return "legal/politique";
    }
    @GetMapping("/conditionsUtilisation")
    public String conditions() {
        return "legal/cu";
    }
}
