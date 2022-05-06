package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.*;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.SetService;
import com.molkky.molkky.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import type.SetTeamIndex;

import javax.servlet.http.HttpSession;
import java.util.Arrays;

@Controller
public class MatchController {
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
        UserLogged user = (UserLogged)session.getAttribute("user");
        model.addAttribute("user", user);
        Match match = matchRepository.findById(id);
        if(user == null){
            return "redirect:/connexion";
        }

//
//        if(!Objects.equals(user.getTeam().getId(), match.getTeams().get(0).getId())
//                && !Objects.equals(user.getTeam().getId(), match.getTeams().get(1).getId())
//        ) {
//            return new ResponseEntity<>("Pas dans une des équipes", HttpStatus.UNAUTHORIZED);
//        }
        SetTeamIndex setTeamIndex = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), UserService.createUserModelFromUserLogged(user));
        model.addAttribute("match", MatchService.getMatchModelFromEntity(match));
        model.addAttribute("teams", Arrays.asList(new TeamModel(match.getTeams().get(0)), new TeamModel(match.getTeams().get(1))));
        model.addAttribute("court", new CourtModel(match.getCourt()));
        model.addAttribute("tournament", new TournamentModel(match.getRound().getTournament()));
        model.addAttribute("sets", SetService.createSetModels(match.getSets()));
        model.addAttribute("setTeamIndex", setTeamIndex);
        return "match/match";
    }

    @PostMapping("/matches/match")
    public String updateSet(Model model, @RequestBody SetModel setModel, HttpSession session) {
        User user = (User)session.getAttribute("user");
        if(Boolean.FALSE.equals(setService.isUserInSet(setModel, UserService.createUserModel(user)))) {
            return "not in match";
        }



//        matchRepository.save(match);
        return "match/match";
    }
}
