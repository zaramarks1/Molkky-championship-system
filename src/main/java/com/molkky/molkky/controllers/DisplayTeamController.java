package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserTournamentRoleCustom;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;


@Controller
@RequestMapping("/team")
public class DisplayTeamController {
    @Autowired
    TeamRepository teamRepository;

    @Autowired
    UserTournamentRoleCustom userTournamentRoleCustom;

    @GetMapping("/displayTeams")
    public String displayTeams(Model model, @RequestParam(value = "filter", required = false) String filter) {
        if (filter != null && !"".equals(filter)) {
            model.addAttribute("teams", teamRepository.searchTeamsByName(filter, 10));
        } else {
            model.addAttribute("teams", teamRepository.findAll());
        }
        return "team/displayTeams";
    }

    @GetMapping("/view")
    public String teamView(Model model, @RequestParam(value = "teamId") String teamId) {
        Team team = teamRepository.findById(Integer.valueOf(teamId));
        model.addAttribute("team", team);
        model.addAttribute("users", userTournamentRoleCustom.findUserByTeam(team));
        return "/team/displayDetailsTeam";
    }
}



