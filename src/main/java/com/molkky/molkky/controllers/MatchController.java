package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.model.TeamModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.SetService;
import com.molkky.molkky.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import type.SetTeamIndex;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.util.Arrays;

@Controller
public class MatchController extends DefaultAttributes {
    @Autowired
    private MatchRepository matchRepository;

    @Autowired
    private SetService setService;

    @Autowired
    private UserService userService;

    @Autowired
    private MatchService matchService;

    @GetMapping("/matches/match")
    public String match(Model model, HttpSession session, @RequestParam(name = "match_id", required = true) Integer id) {
        UserLogged user = (UserLogged)model.getAttribute("user");
        if(user == null){
            return "redirect:/connexion";
        }
        Match match = matchRepository.findById(id);
        SetTeamIndex setTeamIndex = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), UserService.createUserModelFromUserLogged(user));

//        case the user is a player but not in the match
        if(Boolean.TRUE.equals(!matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModelFromUserLogged(user))) && user.getRole() == UserRole.PLAYER){
            return "redirect:/";
        }

        model.addAttribute("match", MatchService.getMatchModelFromEntity(match));
        model.addAttribute("teams", Arrays.asList(new TeamModel(match.getTeams().get(0)), new TeamModel(match.getTeams().get(1))));
        model.addAttribute("court", new CourtModel(match.getCourt()));
        model.addAttribute("tournament", new TournamentModel(match.getRound().getTournament()));
        model.addAttribute("sets", SetService.createSetModels(match.getSets()));
        model.addAttribute("setTeamIndex", setTeamIndex);
        return "/match/match";
    }
}
