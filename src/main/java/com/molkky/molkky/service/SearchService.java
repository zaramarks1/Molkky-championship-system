package com.molkky.molkky.service;

import com.molkky.molkky.model.TeamModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class SearchService {
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    public List<TournamentModel> searchTournaments(String term, Integer n) {
        return TournamentModel.createTournamentModelsFromList(tournamentRepository.searchTournamentsByName(term, n));
    }

    public List<TournamentModel> searchTournaments(String term) {
        return searchTournaments(term, 10);
    }

    public List<UserModel> searchUsers(String term, Integer n) {
        return UserService.createUserModelList(userRepository.searchUsersByName(term, n));
    }

    public List<UserModel> searchUsers(String term) {
        return searchUsers(term, 10);
    }

    public List<TeamModel> searchTeams(String term, Integer n) {
        return TeamModel.createTeamModels(teamRepository.searchTeamsByName(term, n));
    }

    public List<TeamModel> searchTeams(String term) {
        return searchTeams(term, 10);
    }
}
