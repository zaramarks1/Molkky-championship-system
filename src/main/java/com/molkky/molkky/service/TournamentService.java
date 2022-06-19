package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.TournamentStatus;
import type.UserRole;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import static com.molkky.molkky.utility.StringUtilities.createCode;


@Service
public class TournamentService {

    @Autowired
    EmailSenderService emailSenderService;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    UserRepository userRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    RoundService roundService;

    public final Date currentDate = new Date();

    public Boolean isTournamentReady(Tournament tournament) {
        List<Team> totalTeams = tournament.getTeams();
        List<Team> notEliminatedTeams;

        notEliminatedTeams = totalTeams.stream().filter(team -> !team.isEliminated()).collect(Collectors.toList());
        if (tournament.getMinTeam() > notEliminatedTeams.size()) {
            return false;
        }
        if (tournament.getCourts().isEmpty()) {
            return false;
        }
        return !userTournamentRoleRepository.findUserTournamentRoleByRoleAndTournament(UserRole.STAFF, tournament).isEmpty();
    }


    public Tournament create(TournamentModel tournamentModel) {
        Tournament tournament = new Tournament(tournamentModel);

        tournament = tournamentRepository.save(tournament);

        String mail = tournamentModel.getEmail();

        User user = new User();

//        creating the admin
        if (!userRepository.existsUserByEmail(mail)) {
            user.setEmail(mail);
            user.setPassword(createCode(5));
            user = userRepository.save(user);
        } else {
            user = userRepository.findUserByEmail(mail);
        }
        emailSenderService.sendEmail(mail, "Bienvenue sur Molkky", "Bonjour,\n\n" +
                "Vous avez bien créé un tournoi sur Molkky.\n" +
                "Votre mot de passe est : " + user.getPassword() + "\n\n" +
                "Bon jeu sur Molkky !\n\n" +
                "L'équipe Molkky");

        UserTournamentRole userTournamentRole = new UserTournamentRole();

        userTournamentRole.setUser(user);
        userTournamentRole.setRole(UserRole.ADM);
        userTournamentRole.setTournament(tournament);

        userTournamentRoleRepository.save(userTournamentRole);

        return tournament;
    }

    public Tournament modifyTournament(TournamentModel tournamentModel) {
        Tournament tournament = tournamentRepository.findById(tournamentModel.getId());
        tournament.editTournamentInfo(tournamentModel);
        return tournamentRepository.save(tournament);
    }

    // Fonction qui test si le nombre d'équipes inscrites après la date limite d'inscription est suffisant. Sinon, ferme le tournament
    public void closeTournamentWhenMinimumTeamsBeforeDate() {
        List<Tournament> tournaments = tournamentRepository.findAll();

        for (Tournament tournament : tournaments) {
            if (currentDate.after(tournament.getCutOffDate()) && tournament.getTeams().size() <= tournament.getMinTeam()) {
                tournament.setStatus(TournamentStatus.CLOSED);
            }
        }
    }

    // Test quand les inscriptions du tournoi doivent être fermées
    public void registerClosedForTournament() {
        List<Tournament> tournaments = tournamentRepository.findAll();

        for (Tournament tournament : tournaments) {
            if (tournament.getStatus() == TournamentStatus.AVAILABLE && isTournamentNotAvailable(tournament)) {
                tournament.setRegisterAvailable(false);
                tournamentRepository.save(tournament);
            }
        }
    }

    private boolean isTournamentNotAvailable(Tournament tournament) {
        return new Date().after(tournament.getCutOffDate()) || tournament.getMaxTeam() == tournament.getTeams().size();
    }

    // Récupère le gagnant du tournoi
    // Format return list car possibilité qu'il y ait plusieurs gagnants pas écartée pour le moment
    public List<Team> getWinners(Tournament tournament) {

        int nbPhases = tournament.getPhases().size() - 1;
        List<Team> teams = new ArrayList<>();
        List<PhaseRankingModel> phaseRankingModels;
        for (int i = nbPhases; i >= 0; i--) {
            Phase phase = tournament.getPhases().get(i);
            phaseRankingModels = roundService.orderTeamsByScoreInPhase(phase, phase.getVictoryValue());

            for (PhaseRankingModel r : phaseRankingModels) if (!teams.contains(r.getTeam())) teams.add(r.getTeam());
        }
        return teams;
    }

    public String getEmailAdmin(Tournament tournament) {
        List<UserTournamentRole> userTournamentRoles = userTournamentRoleRepository.findUserTournamentRoleByRoleAndTournament(UserRole.ADM, tournament);
        return userTournamentRoles.get(0).getUser().getEmail();
    }

    // EN ATTENTE
    // Lance le tournoi si la date de ce dernier est celle du jour
    public void defineMatchInProgress() {
        List<Tournament> tournaments = tournamentRepository.findAll();

        for (Tournament tournament : tournaments) {
            if (currentDate.after(tournament.getDate())) {
                tournament.setStatus(TournamentStatus.INPROGRESS);
                tournament.setRegisterAvailable(false);
            }
        }
    }
}
