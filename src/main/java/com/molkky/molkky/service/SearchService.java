package com.molkky.molkky.service;

import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.SearchRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class SearchService {
    @Autowired
    private SearchRepository searchRepository;

    public List<TournamentModel> searchTournaments(String term) {
        return TournamentModel.createTournamentModelsFromList(searchRepository.searchTournamentsByName(term));
    }
}
