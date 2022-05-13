package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class InfosController extends DefaultAttributes {

    @GetMapping("/infos")
    public String index(){
        return "infos";
    }
}
