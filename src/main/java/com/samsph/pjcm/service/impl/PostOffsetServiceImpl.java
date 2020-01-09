package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.dao.PostNoOffsetRepository;
import com.samsph.pjcm.model.PostNoOffset;
import com.samsph.pjcm.service.PostNoOffsetService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Optional;


/**
 * @author hujiahao
 */
@Service
public class PostOffsetServiceImpl implements PostNoOffsetService {

    @Resource
    private PostNoOffsetRepository postNoOffsetRepository;

    @Override
    public int getOffsetAndIncrease(int year) {
        Optional<PostNoOffset> optional = postNoOffsetRepository.findByYear(year);
        PostNoOffset postNoOffset;

        postNoOffset = optional.orElseGet(
                () -> new PostNoOffset(null, year, 0));

        return postNoOffsetRepository.save(postNoOffset).getOffset();
    }




}
