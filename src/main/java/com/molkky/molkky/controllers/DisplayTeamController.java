package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.model.TeamFilterModel;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;



@Controller
@RequestMapping("/team")
public class DisplayTeamController extends DefaultAttributes {
    @Autowired
    TeamRepository teamRepository;

    @GetMapping("/displayTeams")
    public String displayTeams(Model model, @RequestParam(value = "filter", required = false) String filter) {
        if(filter != null && !"".equals(filter)){
            model.addAttribute("teams" , teamRepository.searchTeamsByName(filter, 10));
        } else {
            model.addAttribute("teams" , teamRepository.findAll());
        }
        return "team/displayTeams";
    }
}



