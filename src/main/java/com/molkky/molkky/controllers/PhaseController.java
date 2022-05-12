package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.PhaseService;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpSession;
import java.util.HashMap;
import java.util.List;

@Controller
@RequestMapping("/phase")
public class PhaseController {

    @Autowired
    PhaseService phaseService;
    @GetMapping("/generate")
    public HashMap<Round, List<Match>> generate(Model model, HttpSession session, @RequestParam(name = "phase_id", required = true) String id){


        HashMap<Round, List<Match>> response = phaseService.generate(id);

        model.addAttribute("round_match", response);

        return response;

    }


}
