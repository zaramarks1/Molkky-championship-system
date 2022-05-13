package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.TeamModel;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TeamRepository;
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
import java.util.List;

@Controller
public class MatchController extends DefaultAttributes {
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private MatchService matchService;

    private String redirectionMatches = "match/allMatches";

    private String matchAttribute = "matches";


    @GetMapping("/matches/match")
    public String match(Model model, HttpSession session, @RequestParam(name = "match_id", required = true) Integer id) {
        UserLogged user = getUser(session);
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
        return "match/match";
    }

    @GetMapping("/match/allMatches")
    public String matches(Model model, HttpSession session) {
        UserLogged user = getUser(session);
        if (user.getRole().equals(UserRole.PLAYER)){
            TeamModel teamModel = user.getTeam();
            Team team = teamRepository.findById(teamModel.getId());
            List<Match> matchesPlayer= matchRepository.findMatchesByTeams(team);
            model.addAttribute(matchAttribute, matchesPlayer);
        }
        else if (user.getRole().equals(UserRole.STAFF)){
            Tournament tournament = user.getTournament();
            List<Match> matchesStaff = matchRepository.findMatchAttributedToStaff(tournament);
            model.addAttribute(matchAttribute, matchesStaff);
        }
        return redirectionMatches;
    }

    @GetMapping("/match/inProgressMatches")
    public String notFinishedMatches(Model model, HttpSession session) {
        UserLogged user = getUser(session);
        if (user.getRole().equals(UserRole.PLAYER)){
            TeamModel teamModel = user.getTeam();
            Team team = teamRepository.findById(teamModel.getId());
            List<Match> matchesPlayer= matchRepository.findMatchesByTeamsAndFinished(team,false);
            model.addAttribute(matchAttribute, matchesPlayer);

        }
        else if (user.getRole().equals(UserRole.STAFF)){
            Tournament tournament = user.getTournament();
            List<Match> matchesStaffNotFinished = matchRepository.findMatchAttributedToStaffAndFinished(tournament,false);
            model.addAttribute(matchAttribute, matchesStaffNotFinished);
        }
        return redirectionMatches;
    }
    @GetMapping("/match/finishedMatches")
    public String finishedMatches(Model model, HttpSession session) {
        UserLogged user = getUser(session);
        if (user.getRole().equals(UserRole.PLAYER)){
            TeamModel teamModel = user.getTeam();
            Team team = teamRepository.findById(teamModel.getId());
            List<Match> matchesPlayer= matchRepository.findMatchesByTeamsAndFinished(team,true);
            model.addAttribute(matchAttribute, matchesPlayer);

        }
        else if (user.getRole().equals(UserRole.STAFF)){
            Tournament tournament = user.getTournament();
            List<Match> matchesStaffNotFinished = matchRepository.findMatchAttributedToStaffAndFinished(tournament,true);
            model.addAttribute(matchAttribute, matchesStaffNotFinished);
        }
        return redirectionMatches;
    }
}
