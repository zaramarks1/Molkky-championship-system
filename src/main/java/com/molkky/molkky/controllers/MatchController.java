package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.model.*;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Controller
public class MatchController extends DefaultAttributes {
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private MatchService matchService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    private String redirectionMatches = "match/allMatches";

    private String matchAttribute = "matches";


    @GetMapping("/matches/match")
    public String match(Model model, HttpSession session, @RequestParam(name = "match_id", required = true) Integer id) {
        UserLogged user = getUser(session);
        if(user == null){
            return "redirect:/connexion";
        }
        Match match = matchRepository.findById(id);
        SetTeamIndex setTeamIndex = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), new UserTournamentRoleModel(userTournamentRoleRepository.findById(user.getTournamentRoleId())));

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
            User staff = userRepository.findById(user.getId());
            List<Match> matchesStaff = matchRepository.findMatchAttributedToStaff(tournament,staff);
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
            User staff = userRepository.findById(user.getId());
            List<Match> matchesStaffNotFinished = matchRepository.findMatchAttributedToStaffAndFinished(tournament,staff,false);
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
            User staff = userRepository.findById(user.getId());
            List<Match> matchesStaffNotFinished = matchRepository.findMatchAttributedToStaffAndFinished(tournament,staff,true);
            model.addAttribute(matchAttribute, matchesStaffNotFinished);
        }
        return redirectionMatches;
    }

    @GetMapping("/match/validateMatch")
    public String validateMatches(Model model, HttpSession session) {
        UserLogged user = getUser(session);
        Tournament tournament = user.getTournament();
        User staff = userRepository.findById(user.getId());
        List<Match> matchesStaff = matchRepository.findMatchAttributedToStaff(tournament,staff);
        List<Match> matchIncorrectScore = new ArrayList<>();
        List<Integer> setIncorrectScore = new ArrayList<>();
        int i = 1;
        for (Match match : matchesStaff) {
            for (Set set : match.getSets()){
                if((!set.getScore1Team1().equals(set.getScore1Team2())||(!set.getScore2Team1().equals(set.getScore2Team2())))){
                    matchIncorrectScore.add(match);
                    setIncorrectScore.add(i);
                }
                i=i+1;
            }
        }
        model.addAttribute(matchAttribute, matchIncorrectScore);
        model.addAttribute("incorrectSet", setIncorrectScore);
        return redirectionMatches;
    }
}
