package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/team")
public class TeamController {

    @Autowired
    TeamRepository teamRepository;
    @Autowired
    TournamentRepository tournamentRepository;


    @GetMapping("/create")
    public String create(Model model){
        model.addAttribute("tournaments", tournamentRepository.findAll());
        model.addAttribute("team", new CreateTeamModel());
        return "/team/create";
    }

    /*@PostMapping("/create")
        public String submit(@ModelAttribute("team") CreateTeamModel team, Model model){


        Integer idTournament = Integer.valueOf(team.getTournament());
        Tournament tournament = tournamentRepository.findById(idTournament);

        //System.out.println(team.getTournament().getName());
        Team teamCreate = new Team();

        teamCreate.setName(team.getName());
        teamCreate.setNbPlayers(team.getNbPlayers());
        teamCreate.setTournament(tournament);

        Team teamNew = teamRepository.save(teamCreate);

        model.addAttribute("team", teamNew);

        return "redirect:/team/"+teamNew.getId()+"/addPlayer" ;
        }*/

    @PostMapping("/create")
    public ModelAndView submit(@ModelAttribute("team") CreateTeamModel team, ModelMap model ){


        Integer idTournament = Integer.valueOf(team.getTournament());
        Tournament tournament = tournamentRepository.findById(idTournament);

        //System.out.println(team.getTournament().getName());
        Team teamCreate = new Team();

        teamCreate.setName(team.getName());
        teamCreate.setNbPlayers(team.getNbPlayers());
        teamCreate.setTournament(tournament);

        Team teamNew = teamRepository.save(teamCreate);

        model.addAttribute("team", teamNew);
        AddPlayerlistModel players = new AddPlayerlistModel();
        for(int i =0 ; i< teamNew.getNbPlayers();i++){
            players.addPlayer(new AddPlayerModel());
        }
        model.addAttribute("form", players);
        return new ModelAndView( "/team/addPlayer", model) ;
    }

       /*@GetMapping("/{idTeam}/addPlayer")
        public String addPlayerForm(Model model, @PathVariable("idTeam") String teamId){
            System.out.println("entrou");
            Team team = teamRepository.findById(Integer.valueOf(teamId));
            model.addAttribute("team2",team);
            List<User> users = new ArrayList<>();

            return "/team/addPlayer";
        }*/

    @GetMapping("/addPlayer")
    public String addPlayerForm(Model model){
        System.out.println("entrou");
        //Team team = teamRepository.findById(Integer.valueOf(teamId));
        //model.addAttribute("team2",team);
        List<User> users = new ArrayList<>();
         Team t  = (Team) model.getAttribute("team");
         System.out.println(t.getName());
        model.addAttribute("users", users);
        return "/team/addPlayer";
    }

    @PostMapping("/addPlayer")
        public String addPlayer(){
            return "";
        }
}