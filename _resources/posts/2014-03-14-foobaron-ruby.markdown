---
title: foo bar on ruby
date: 2014-03-14
description: ruby bla bla
tags: ruby, emacs
---

this post is about ruby

<!--?prettify lang=ruby-->

    # Does commonly used redis persistence actions
    module RedisBlobPersistence

      def persist
        $redis.set(key, value)
      end

      private

      def value
        Marshal::dump(self)
      end

      def self.load (root_node_id)
        Marshal::load($redis.get(root_node_id))
      end

    end
