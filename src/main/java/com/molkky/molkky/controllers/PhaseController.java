package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.service.PhaseService;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/phase")
public class PhaseController {

    @Autowired
    PhaseService phaseService;
    @GetMapping("/{id}/generate")

    public void generate(@PathVariable String id){

        phaseService.generate(id);

    }


}
